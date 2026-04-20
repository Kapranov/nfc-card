-module(kaukonahua_consume).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-behaviour(gen_server).

-include_lib("brod/include/brod.hrl").

-define(SERVER,?MODULE).

-export([code_change/3
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_message/4
        ,init/1
        ,init/2
        ,start_link/0
        ,terminate/2
        ]).

-record(consumer_state, {}).

-spec start_link() -> {ok,pid()} | ignore | {error,{already_started,pid()}}.
start_link() ->
  gen_server:start_link({local,?SERVER},?MODULE,[],[]).

-spec init(list()) -> {ok,map()} |
                     {ok,map(),non_neg_integer} |
                     ignore |
                     {stop,atom()}.
init([]) ->
  Topic = <<"test-topic">>,
  GroupConfig = [
    {offset_commit_policy,commit_to_kafka_v2},
    {offset_commit_interval_seconds,5}
  ],
  GroupId = <<"test_group_id">>,
  ConsumerConfig=[{begin_offset,earliest}],
  brod:start_link_group_subscriber(
    client1,
    GroupId,
    [Topic],
    GroupConfig,
    ConsumerConfig,
    _CallbackModule=?MODULE,
    _CallbackInitArg=[]
  ),
  {ok,#consumer_state{}}.
-spec init(any(),any()) -> {ok,list()} |
                     ignore |
                     {stop,atom()}.
init(_GroupId,_Opts) -> {ok,[]}.

handle_message(_Topic,_Partition,Message,State) ->
  #kafka_message{offset=_Offset,key=_Key,value=_Value}=Message,
  io:fwrite("Star Advertiser news ~p ~n",[Message]),
  {ok,ack,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

