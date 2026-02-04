-module(kilauea).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([code_change/3
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,start_link/0
        ,terminate/2
        ]).

-export([loop/1,start/1,stop/1,get_option/2]).
-export([request/2,request/5]).

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

start_link() ->
  {ok,Pid}=gen_server:start_link({local,?MODULE},?MODULE,[],[]),
  io:format("MochiWeb HTTP Server started with pid: ~p~n",[Pid]),
  {ok,Pid}.

init([]) -> {ok,[]}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

-spec get_option(atom(), [{atom(), atom() | boolean() | integer() | string()}]) ->
  {atom() | boolean() | integer() | string(), list() | [{atom(), atom() | boolean() | integer() | string()}]}.
get_option(Option,Options) ->
  {proplists:get_value(Option,Options),
   proplists:delete(Option,Options)}.

-spec start(list()) -> pid().
start(Opts) ->
  application:start(inets),
  {Name, Opts1} = get_option(name, Opts),
  {Port, _} = get_option(port, Opts1),
  Args = [{name, Name}, {port, Port}, {loop, fun loop/1} | []],
  {ok, Pid} = mochiweb_http:start(Args),
  Pid.

-spec stop(atom()) -> ok.
stop(Name) -> mochiweb_http:stop(Name).

-spec loop(tuple()) -> tuple().
loop(Req) ->
  "/" ++ Path = mochiweb_request:get(path,Req),
  try case mochiweb_request:get(method,Req) of
        Method when Method =:= 'GET';
                    Method =:= 'HEAD' ->
          case Path of
            "hello_world" ->
              %mochiweb_request:respond({200,[{"Content-Type","text/html"}],["<html><body>Hello</body></html>"]},Req).
              %mochiweb_request:respond({200,[{"Content-Type","text/plain"}],"Hello World"},Req).
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
  do_request(Method, {Uri, Headers, ContentType, Body}).

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

