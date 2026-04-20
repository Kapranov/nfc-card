-module(sdp_query).
-export([get/1,get/2,run/2]).
-export([create_users/2,drop_users/0,update_users/1,show_users/1]).

-record(users,{id,email,pass,fname,lname,active,created_at,updated_at,deleted_at}).

get(Name) ->
  persistent_term:get({?MODULE,Name}).

get(Name,Params) ->
  lists:map(fun(Key) when is_atom(Key) ->
                    proplists:get_value(Key,Params);
               (S) ->
                    S
            end, ?MODULE:get(Name)).

run(QueryName,Args) ->
  pgo:query(?MODULE:get(QueryName),Args).

-spec create_users(string(),string()) -> {atom(), string()}.
create_users(Email,Pass) ->
  Schema = pgo:query("create table users ( id SERIAL PRIMARY KEY,
                      email VARCHAR(256) NOT NULL,
                      pass VARCHAR(64) NOT NULL,
                      fname VARCHAR(256),
                      lname VARCHAR(256),
                      active BOOLEAN NOT NULL DEFAULT FALSE,
                      created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
                      updated_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                      deleted_at TIMESTAMP WITH TIME ZONE DEFAULT NULL)"),
  case Schema of
    {error,{pgsql_error, _}} -> {error, "table users exist or does not exist"};
    #{command := create} ->
      #{command := create} = pgo:query("create index users_email_active_idx on users(email, active)"),
      #{command := create} = pgo:query("create index users_email_pass_active_idx on users(email, pass, active)"),
      #{command := insert} = pgo:query("insert into users (email, pass) VALUES ($1, $2)",[Email,Pass]),
      {ok, "created table users"}
  end.

-spec update_users(integer()) -> {atom(), string()}.
update_users(UserId) ->
  case pgo:query("update users set active = true where id = $1::integer",[UserId]) of
    #{command := update,rows := [],num_rows := 0} ->
      {ok, "updated without changes"};
    #{command := update,rows := [],num_rows := 1} ->
      {ok,"updated table users"}
  end.

-spec show_users(integer()) -> {atom(), string() | map()}.
show_users(UserId) ->
  case pgo:query("select * from users where id = $1::integer",[UserId]) of
    #{command := select,rows := [],num_rows := 0} ->
      {error, "users does not exist"};
    #{command := select,rows := [{UserId,Email,Pass,FName,LName,Active,CreatedAt,UpdatedAt,DeletedAt}],num_rows := 1} ->
      {ok,#users{id=UserId,email=Email,pass=Pass,fname=FName,lname=LName,active=Active,created_at=CreatedAt,updated_at=UpdatedAt,deleted_at=DeletedAt}}
  end.

-spec drop_users() -> {atom(), string()}.
drop_users() ->
  case pgo:query("drop table users") of
    {error,{pgsql_error, _}} ->
      {error, "table users does not exist"};
    #{command := drop} ->
      {ok, "deleted table users"}
  end.
