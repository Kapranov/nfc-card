-module(http_call).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([dispatch_request/2]).
-export([amqp_config/0
        ,code_change/3
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,start_link/3
        ,terminate/2
        ]).
-export([body_length_headers/1,client_headers/2,random_binary/2,random_id/1,random_nchar/0,random_onechar/0]).
-export([start_server/0, responder/1,fire/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-define(SERVER,?MODULE).
-define(SAVE_BODY, <<"Hello World!">>).

-record(state,{channel}).

-spec amqp_config() -> [tuple()].
amqp_config() ->
  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
  {ok,RabbitHost}=application:get_env(server,rabbit_host),
  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
  {ok,RabbitPort}=application:get_env(server,rabbit_port),
  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
  [{connection_timeout,RabbitConnectionTimeout}
  ,{heartbeat,RabbitHeartbeat}
  ,{host,RabbitHost}
  ,{password,RabbitPassword}
  ,{port,RabbitPort}
  ,{ssl_options,RabbitSSLOptions}
  ,{username,RabbitUsername}
  ,{virtual_host,RabbitVirtualHost}
  ].

dispatch_request(_,Req) ->
  gen_server:call({local,?SERVER},{request,Req}),
  ok.

start_link(Config,Exchange,Type) when is_list(Config);
                                      is_binary(Exchange);
                                      is_binary(Type) ->
  {ok,Pid}=gen_server:start_link({local,?SERVER},?SERVER,[Config,Exchange,Type],[]),
  io:format("HTTP Server started with pid: ~p~n",[Pid]),
  {ok,Pid}.

init([Config,Name,Type]) ->
  {ok,Channel}=amqp_options:start_rabbitmq_channel(Config,Name,Type),
  {ok,#state{channel=Channel}}.

handle_call({request,Req},_,State=#state{channel=Channel}) ->
  Method=Req:get(method),
  Path=Req:get(raw_path),
  Body=Req:recv_body(),
  Response=handle_request(Method,Path,Body,Channel),
  Req:respond(Response),
  Req:cleanup(),
  {reply,ok,State};
handle_call(_Msg,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,#state{channel=Channel}) ->
  amqp_connection:close(Channel),
  ok;
terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

handle_request('GET',"/http/api/test",_,Channel) ->
  publish_message(<<"test">>,"test",Channel).

publish_message(_,_,_) ->
  {404,[{"Content-Type","text/plain"}],<<"Message Not Defined">>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec body_length_headers(string()) -> list().
body_length_headers(Body) ->
    ["Content-Length: ", integer_to_list(byte_size(Body)), "\r\n"].

-spec client_headers(string(),list()) -> list().
client_headers(Body,IsLastRequest) ->
  ["Host: localhost\r\n",
   case Body of
     <<>> ->
       "";
     _ ->
       ["Content-Type: application/json\r\n" | body_length_headers(Body)]
   end,
   case IsLastRequest =:= [] of
     true ->
       "Connection: close\r\n";
     false ->
       ""
   end].

-spec random_binary(integer(), integer()) -> binary().
random_binary(ShortInt,LongInt) ->
  << <<(rand:uniform(256) - 1)>> || _ <- lists:seq(1, ShortInt + rand:uniform(1 + LongInt - ShortInt) - 1) >>.

-spec random_id(binary()) -> binary().
random_id(Data) ->
  mochiweb_base64url:decode(mochiweb_base64url:encode(Data)).

-spec random_nchar() -> [string()].
random_nchar() ->
  [random_id(B) || _ <- lists:seq(1,3), B <- [random_binary(2, 6)]].

-spec random_onechar() -> [string()].
random_onechar() ->
  [random_id(<<C>>) || C <- lists:seq(0,255)].

fire() ->
  URL = "http://127.0.0.1:8080/api",
  Method = get,
  %Headers = [{"content-type", "text/plain"}, {"date", "Sun, 11 Jan 2026 05:07:19"}],
  Headers = [{"content-type", "text/plain"}],
  httpc:request(Method, {URL, Headers}, [], []).

start_server() ->
  ets:new(my_table,[named_table,public,set,{keypos, 1}]),
  mochiweb_http:start([{port,8080},{loop, {?MODULE,responder}}]).

%%% Options = [{ip, {127,0,0,1}}, {port, 8080}, {docroot, "tmp"}].
%%% mochiweb:start(Options).
%%% [httpc:request(post, {"http://localhost:8080/" ++ Path, [], "application/json", "{}"}, [], []) || _ <- lists:seq(1, 10), Path <- ["begin_url", "end_url"]].
%%%
%%%
%%%

%%% Headers = mochiweb_headers:make([{"Content-Type", "text/plain"}]).
%%% Req = mochiweb_request:new(testing, 'GET', "/foo", {1, 1}, Headers).
%%% mochiweb_request:respond({201,[{"Content-Type", "text/plain"}], <<>>},Req).
%%% mochiweb_request:ok({"text/plain", Headers, "/foo"}, Req).

%%% rabbitmq-mochiweb/src/rabbit_web_dispatch_registry.erl
%%%
%%% -define(ETS, rabbitmq_web_dispatch).
%%% init([]) ->
%%%   ?ETS = ets:new(?ETS, [named_table, public]),
%%%   {ok, undefined}.
%%%
%%% port(Listener) -> proplists:get_value(port, Listener).
%%%
%%% lookup_dispatch(Lsnr) ->
%%%    case ets:lookup(?ETS, port(Lsnr)) of
%%%        [{_, Lsnr, S, F}]   -> {ok, {S, F}};
%%%        [{_, Lsnr2, S, _F}] -> {error, {different, first_desc(S), Lsnr2}};
%%%        []                  -> {error, {no_record_for_listener, Lsnr}}
%%%    end.
%%% first_desc([{_N, _S, _H, {_, Desc}} | _]) -> Desc.
%%%
%%% list() ->
%%%    [{Path, Desc, Listener} ||
%%%        {_P, Listener, Selectors, _F} <- ets:tab2list(?ETS),
%%%        {_N, _S, _H, {Path, Desc}} <- Selectors].
%%%
%%% listener_by_name(Name) ->
%%%    case [L || {_P, L, S, _F} <- ets:tab2list(?ETS), contains_name(Name, S)] of
%%%        [Listener] -> Listener;
%%%        []         -> exit({not_found, Name})
%%%    end.
%%%
%%% contains_name(Name, Selectors) ->
%%%    lists:member(Name, [N || {N, _S, _H, _L} <- Selectors]).
%%%
%%% list(Listener) ->
%%%    {ok, {Selectors, _Fallback}} = lookup_dispatch(Listener),
%%%    [{Path, Desc} || {_N, _S, _H, {Path, Desc}} <- Selectors].
%%%
%%%
%%% terminate(_, _) ->
%%%   true = ets:delete(?ETS),
%%%   ok.
%%%
%%% lookup(Listener, Req) ->
%%%    case lookup_dispatch(Listener) of
%%%        {ok, {Selectors, Fallback}} ->
%%%            case catch match_request(Selectors, Req) of
%%%                {'EXIT', Reason} -> {lookup_failure, Reason};
%%%                no_handler       -> {handler, Fallback};
%%%                Handler          -> {handler, Handler}
%%%            end;
%%%        Err ->
%%%            Err
%%%    end.
%%%
%%% lookup_dispatch(Lsnr) ->
%%%    case ets:lookup(?ETS, port(Lsnr)) of
%%%        [{_, Lsnr, S, F}]   -> {ok, {S, F}};
%%%        [{_, Lsnr2, S, _F}] -> {error, {different, first_desc(S), Lsnr2}};
%%%        []                  -> {error, {no_record_for_listener, Lsnr}}
%%%    end.
%%% first_desc([{_N, _S, _H, {_, Desc}} | _]) -> Desc.
%%%


responder(Req) ->
  %mochiweb_request:respond({200,[{"Content-Type","text/html"}],["<html><body>Hello</body></html>"]},Req).
  case ets:lookup(my_table, some_key) of
    [{_,Value}] -> Req:ok({"text/plain", Value});
    [] -> Req:not_found()
  end.
