-module(mojtaba).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-include_lib("webmachine/include/webmachine.hrl").

-export([allowed_methods/2
        ,content_types_accepted/2
        ,content_types_provided/2
        ,from_json/2
        ,init/1
        ,process_post/2
        ,to_json/2
        ]).

init([]) -> {ok,undefined}.

allowed_methods(ReqData,State) ->
  {['DELETE','GET','HEAD','OPTIONS','POST','PUT','TRACE'],ReqData,State}.

content_types_accepted(ReqData,State) ->
  {[{"application/json"},from_json],ReqData,State}.

content_types_provided(ReqData,State) ->
  {[{"application/json",to_json}], ReqData,State}.

process_post(ReqData,State) ->
  [{Json,_}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  ReqData2 = wrq:set_resp_body(Json,ReqData),
  {true,ReqData2,State}.

to_json(ReqData,State) ->
  {false,ReqData,State}.

from_json(ReqData,State) ->
  {false,ReqData,State}.

%%% resp_body/1
%%% resp_headers/1
%%% req_headers/1

%-behaviour(gen_server).

%-export([code_change/3
%        ,handle_call/3
%        ,handle_cast/2
%        ,handle_info/2
%        ,init/1
%        ,start_link/0
%        ,terminate/2
%        ]).
%-export([web_config/0]).
%
%-spec web_config() -> [tuple()].
%web_config() ->
%  [
%   {name, ?MODULE},
%   {ip, "127.0.0.1"},
%   {port, 8080},
%   {dispatch, dispatch()}
%  ].
%
%-spec dispatch() -> [webmachine_dispatcher:route()].
%dispatch() ->
%  lists:flatten([{["demo", '*'], webmachine_demo_resource, []}]).
%
%start_link() ->
%  {ok,Pid}=gen_server:start_link({local,?MODULE},?MODULE,[],[]),
%  io:format("WebMachine HTTP Server started with pid: ~p~n",[Pid]),
%  {ok,Pid}.
%
%init([]) -> {ok,[]}.
%
%handle_call(_Request,_From,State) -> {reply,ok,State}.
%
%handle_cast(_Msg,State) -> {noreply,State}.
%
%handle_info(_Info,State) -> {noreply,State}.
%
%terminate(_Reason,_State) -> ok.
%
%code_change(_OldVsn,State,_Extra) -> {ok,State}.

