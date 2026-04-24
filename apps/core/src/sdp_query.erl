-module(sdp_query).
-export([get/1,get/2,run/2]).
-export([create_users/2,drop_users/0,update_users/2,show_users/1]).

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

-spec update_users(integer(),list()) -> {atom(), string()}.
update_users(UserId,UsersParam) ->
  %Email = proplists:get_value(Email,UsersParam.,
  %Pass = proplists:get_value(pass,UsersParam),
  %Fname = proplists:get_value(fname,UsersParam),
  %Lname = proplists:get_value(lname,UsersParam),
  %Active = proplists:get_value(active,UsersParam),
  %CreatedAt = proplists:get_value(created_at,UsersParam),
  %UpdatedAt = proplists:get_value(updated_at,UsersParam),
  %DeletedAt = proplists:get_value(deleted_at,UsersParam),
  UserList = [proplists:get_value(F,UsersParam) || F <- [email,pass,fname,lname,active]],
  [Email,Pass,Fname,Lname,Active] = UserList,
  case Email of
    undefined ->
      case Pass of
        undefined ->
          case Fname of
            undefined ->
              case Lname of
                undefined ->
                  case Active of
                    undefined ->
                      {ok, "without updated, params are undefined"};
                    _ -> pgo:query("update users set active = $2::boolean where id = $1::integer",[UserId,Active])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set lname = $2::string where id = $1::integer",[UserId,Lname]);
                    _ -> pgo:query("update users set lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname])
                  end
              end;
            _ ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set fname = $2::string where id = $1::integer",[UserId,Fname]);
                    _ -> pgo:query("update users set fname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Fname])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set fname = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Fname]);
                    _ -> pgo:query("update users set fname = $4::string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Fname])
                  end
              end
          end;
        _ ->
          case Fname of
            undefined ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set pass = $2::string where id = $1::integer",[UserId,Pass]);
                    _ -> pgo:query("update users set pass = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Pass])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set pass = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Pass]);
                    _ -> pgo:query("update users set pass = $4::string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Pass])
                  end
              end;
            _ ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set pass = $3::string, fname = $2::string where id = $1::integer",[UserId,Fname,Pass]);
                    _ -> pgo:query("update users set pass = $4::string, fname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Fname,Pass])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set pass = $4::string, fname = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Fname,Pass]);
                    _ -> pgo:query("update users set pass = $5::string, fname = $4::string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Fname,Pass])
                  end
              end
          end
      end;
    _ ->
      case Pass of
        undefined ->
          case Fname of
            undefined ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set email = $2 where id = $1::integer",[UserId,Email]);
                    _ -> pgo:query("update users set email = $3, active = $2  where id = $1::integer",[UserId,Active,Email])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set email = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Email]);
                    _ -> pgo:query("update users set email = $4::string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Email])
                  end
              end;
            _ ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set email = $3::string, fname = $2::string where id = $1::integer",[UserId,Fname,Email]);
                    _ -> pgo:query("update users set email = $4::string, fname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Fname,Email])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set email = $4::string, fname = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Fname,Email]);
                    _ -> pgo:query("update users set email = $5::string, fname = $4::string, lname = $3::string, active = $2:: boolean where id = $1::integer",[UserId,Active,Lname,Fname,Email])
                  end
              end
          end;
        _ ->
          case Fname of
            undefined ->
              case Lname of
                undefined ->
                  case Active of
                    undefined -> pgo:query("update users set email = $3::string, pass = $2::string where id = $1::integer",[UserId,Pass,Email]);
                    _ -> pgo:query("update users set email = $4::string, pass = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Pass,Email])
                  end;
                _ ->
                  case Active of
                    undefined -> pgo:query("update users set email = $4::string, pass = $3::string, lname = $2::string where id = $1::integer",[UserId,Lname,Pass,Email]);
                    _ -> pgo:query("update users set email = $5::string, pass = $4::string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Pass,Email])
                  end
              end;
            _ ->
              case Lname of
                undefined ->
                  case Active of
                    undefined ->
                      pgo:query("update users set email = $4::string, pass = $3::string, fname = $2:string where id = $1::integer",[UserId,Fname,Pass,Email]);
                    _ ->
                      pgo:query("update users set email = $5::string, pass = $4::string, fname = $3:string, active = $2::boolean where id = $1::integer",[UserId,Active,Fname,Pass,Email])
                  end;
                _ ->
                  case Active of
                    undefined ->
                      pgo:query("update users set email = $5::string, pass = $4::string, fname = $3:string, lname = $2::string where id = $1::integer",[UserId,Lname,Fname,Pass,Email]);
                    _ ->
                      pgo:query("update users set email = $6::string, pass = $5::string, fname = $4:string, lname = $3::string, active = $2::boolean where id = $1::integer",[UserId,Active,Lname,Fname,Pass,Email])
                  end
              end
          end
      end
  end.
%  case pgo:query("update users set active = true where id = $1::integer",[UserId]) of
%    #{command := update,rows := [],num_rows := 0} ->
%      {ok, "updated without changes"};
%    #{command := update,rows := [],num_rows := 1} ->
%      {ok,"updated table users"}
%  end.

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
