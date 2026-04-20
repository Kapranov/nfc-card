-module(kaukonahua_client).
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

-spec start_link() -> {ok,pid()} | ignore | {error,{already_started,pid()}}.
start_link() ->
  gen_server:start_link({local,?SERVER},?MODULE,[],[]).

-spec init(any()) -> {ok,map()} |
                     {ok,map(),non_neg_integer} |
                     ignore |
                     {stop,atom()}.
init(_Args) ->
  {ok,_}=application:ensure_all_started(brod),
  KafkaEndpoints=[{"localhost",9092}],
  ok=brod:start_client(KafkaEndpoints,client1),
  io:format("Starting Kafka Client ~n"),
  {ok,#client_state{}}.

-spec handle_call(atom(),atom(),map()) -> {reply,atom(),map()} |
                                          {reply,atom(),map(),non_neg_integer()} |
                                          no_return() |
                                          {noreply,map(),non_neg_integer()} |
                                          {stop,atom(),atom(),map()} |
                                          {stop,atom(),map()}.
handle_call(_Request,_From,State) -> {reply,ok,State}.

-spec handle_cast(tuple(),map()) -> no_return() |
                                    {noreply,map(),non_neg_integer()} |
                                    {stop,atom(),map()}.
handle_cast(_Msg,State) -> {noreply,State}.

-spec handle_info(atom(),map()) -> no_return() |
                                   {noreply,map(),non_neg_integer()} |
                                   {stop,atom(),map()}.
handle_info(_Info,State) -> {noreply,State}.

-spec terminate(any(),map()) -> ok.
terminate(_Reason,_State) -> ok.

-spec code_change(any(),map(),any()) -> {ok,map()}.
code_change(_OldVsn,State,_Extra) -> {ok,State}.

