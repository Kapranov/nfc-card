-module(http_request).
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

-export([dispatch_request/2,proba/0]).

-define(MQ_ROUTE_TABLE,dict:from_list([{"example",#msgroute{url="http://www.example.com/api/example",timeout=1000}}])).

-record(msgroute,{url,timeout=infinity}).

proba() -> ?MQ_ROUTE_TABLE.

call(Args) ->
  gen_server:call(?MODULE,{request,Args}).

dispatch_request(_,Request) ->
  call(Request),
  ok.

start_link() ->
  {ok,Pid}=gen_server:start_link({local,?MODULE},?MODULE,[],[]),
  io:format("HTTP Server started with pid: ~p~n",[Pid]),
  {ok,Pid}.

init([]) ->
  {ok,[]}.

% webmachine_util:ensure_all_started(mochiweb).
% Req = mochiweb_request:new(testing,'Get',"http://www.example.com/api/example",{1,1},mochiweb_headers:make([])).
% Headers = mochiweb_headers:make([{"Accept","application/json"}]).
% Req = mochiweb_request:new(testing,'Get',"http://www.example.com/api/example",{1,1},Headers).
% Top = <<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD "
% "HTML 2.0//EN\"><html><head><title>301 "
% "Moved Permanently</title></head><body><h1>Mov"
% "ed Permanently</h1><p>The document has "
% "moved <a href=\"">>,
% Bottom = <<">here</a>.</p></body></html>\n">>,
% Body = <<Top/binary, LocationBin/binary, Bottom/binary>>,
% respond({301, MoreHeaders, Body}, THIS):
%
% mochiweb_request:respond({200,[{"Content-Type", "text/html"}],["<html><body>Hello</body></html>"]}, Req).
%
% mochiweb_request:get(method,Req).
% mochiweb_request:get(headers,Req).
% mochiweb_request:get(path,Req).
% mochiweb_request:get(raw_path,Req).

handle_call({request,Req},_From,State) ->
  Method=Req:get(method),
  Path=Req:get(raw_path),
  Body=Req:recv_body(),
  Response=handle_request(Method,Path,Body,State),
  Req:respond(Response),
  Req:cleanup(),
  {reply,ok,State};

handle_call(_Request,_From,State) ->
  {reply,ok,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.

handle_info(_Info,State) ->
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_request('GET', "/api/test", _Body,_State) ->
  publish_message().

publish_message() ->
  {200, [{"Content-Type", "text/plain"}], <<"OK">>}.
