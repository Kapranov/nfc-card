-module(waialua_client).
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

-define(SERVER,?MODULE).

-record(client_state,{}).

start_link() ->
  gen_server:start_link({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
  {ok,_}=application:ensure_all_started(brod),
  KafkaEndpoints=[{"localhost",9092}],
  ok=brod:start_client(KafkaEndpoints,client1),
  io:format("Starting Kafka Client ~n"),
  {ok,#client_state{}}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.
