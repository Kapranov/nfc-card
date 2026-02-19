-module(http_call).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([dispatch_request/2]).
-export([amqp_config/0 ,code_change/3 ,handle_call/3 ,handle_cast/2 ,handle_info/2 ,init/1 ,start_link/3 ,terminate/2 ]).
-export([body_length_headers/1,client_headers/2,random_binary/2,random_id/1,random_nchar/0,random_onechar/0]).
-export([start/1,stop/1,get_option/2]).
-export([request/2,request/3,request/5,retrieve/3]).

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

-ifdef(OTP_RELEASE).
-if((?OTP_RELEASE) >= 21).
-define(HAS_DIRECT_STACKTRACE, true).
-endif.
-endif.
-ifdef(HAS_DIRECT_STACKTRACE).
-define(CAPTURE_EXC_PRE(Type,What,Trace),Type:What:Trace).
-define(CAPTURE_EXC_GET(Trace),Trace).
-else.
-define(CAPTURE_EXC_PRE(Type,What,Trace),Type:What).
-define(CAPTURE_EXC_GET(Trace),erlang:get_stacktrace()).
-endif.

-spec get_option(atom(),[{atom(), boolean() | integer() | string()}]) -> {atom() | boolean() | integer() | string(), list() | [{atom(), boolean() | integer() | string()}]}.
get_option(Option,Options) ->
  {proplists:get_value(Option,Options),
   proplists:delete(Option,Options)}.

-spec start(list()) -> pid().
start(Opts) ->
  application:start(inets),
  {Name,Opts1}=get_option(name,Opts),
  {Port,Opts2}=get_option(port,Opts1),
  {Ip,Opts3}=get_option(ip,Opts2),
  {Backlog,Opts4}=get_option(backlog,Opts3),
  {Delay,Opts5}=get_option(nodelay,Opts4),
  {Pool,Opts6}=get_option(acceptor_pool_size,Opts5),
  {Ssl,Opts7}=get_option(ssl,Opts6),
  {Profile,Opts8}=get_option(profile_fun,Opts7),
  {Link,Opts9}=get_option(link,Opts8),
  {Recbuf,_}=get_option(recbuf,Opts9),
  Args=[{name,Name},{port,Port},{ip,Ip},{backlog,Backlog},{nodelay,Delay},{acceptor_pool_size,Pool},{ssl,Ssl},{profile_fun,Profile},{link,Link},{recbuf, Recbuf},{loop, fun responder/1} | []],
  mochiweb_http:start(Args).

-spec responder(tuple()) -> tuple().
responder(Req) ->
  "/" ++ Path = mochiweb_request:get(path,Req),
  try case mochiweb_request:get(method,Req) of
        Method when Method =:= 'GET';
                    Method =:= 'HEAD' ->
          case Path of
            "hello_world" ->
              io:format("!!! MochiWeb return request !!! => ~p~n",[Req]),
              mochiweb_request:respond({200,[{"Content-Type","application/json"}],"{\"message\":  \"Hello World\"}"},Req);
            _ ->
              mochiweb_request:serve_file(Path,[],Req)
          end;
        'POST' ->
          case Path of
            _ -> mochiweb_request:not_found(Req)
          end;
        _ ->
          mochiweb_request:respond({501,[],[]},Req)
      end
  catch ? CAPTURE_EXC_PRE ( Type , What , Trace ) ->
          Report=["web request failed",{path,Path},{type,Type},{what,What},{trace, ? CAPTURE_EXC_GET ( Trace )}],
          error_logger:error_report(Report),
          mochiweb_request:respond({500,[{"Content-Type","text/plain"}],"request failed, sorry\n"},Req)
  end.

-spec request(atom(),string()) -> tuple().
request(Method, Uri) ->
  request(Method, Uri, []).

-spec request(atom(), string(), list()) -> tuple().
request(Method, Uri, Headers) ->
  do_request(Method, {Uri, Headers}).

-spec request(atom(), string(), list(), string(), string()) -> tuple().
request(Method, Uri, Headers, ContentType, Body) ->
  case Method of
    Method when Method =:= 'PUT'; Method =:= 'POST'; Method =:= 'DELETE' ->
      do_request(Method, {Uri, Headers, ContentType, Body});
    _ ->
      do_request(Method, {Uri, Headers})
  end.

-spec do_request(atom(), tuple()) -> tuple().
do_request(Method, Request) ->
  handle_response(httpc:request(
    Method,
    Request,
    [{ssl, [{verify, verify_none}]}],
    []
  )).

-spec handle_response(tuple()) -> tuple().
handle_response(Resp) ->
  case Resp of
    {ok, {_, RespHeaders, RespBody}} ->
      {ok, RespHeaders, RespBody};
    {error, _Reason} = E -> E
  end.

-spec stop(atom()) -> ok.
stop(Name) -> mochiweb_http:stop(Name).

-spec retrieve(atom(),string(),[{string(),string()}]) -> tuple().
retrieve(Method,Url,Headers) ->
  httpc:request(Method,{Url,Headers},[],[{body_format, binary}, {full_result, false}]).
