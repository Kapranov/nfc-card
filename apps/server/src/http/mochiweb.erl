-module(mochiweb).
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
