-module(waialua).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([create_topic/1
        ,delete_topics/1
        ,ensure_started/1
        ,fetch/4
        ,get_consumer/3
        ,get_partitions_count/2
        ,get_producer/3
        ,list_all_groups/2
        ,list_groups/2
        ,off/0
        ,start/0
        ,start_link/0
        ,stop/0
        ]).

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

off() ->
  init:stop(),
  halt().

-spec start_link() -> {ok,pid()}.
start_link() ->
  waialua_sup:start_link().

-spec start() -> ok.
start() ->
  application:start(waialua).

-spec stop() -> ok.
stop() ->
  Res=application:stop(waialua),
  Res.

-spec create_topic(binary()) -> tuple().
create_topic(Name) ->
  TopicConfigs = [#{
    name => Name,
    num_partitions => 1,
    replication_factor => 1,
    assignments => [],
    configs => [#{name => <<"cleanup.policy">>,value => "compact"}]
  }],
  ok = brod:create_topics([{"localhost",9092}], TopicConfigs, #{timeout => 1000},[]),
  {ok,Name}.

-spec delete_topics(string()) -> tuple().
delete_topics(Name) ->
  ok = brod:delete_topics([{"localhost",9092}],[Name],5000,[]),
  {ok,Name}.

-spec get_partitions_count(atom(), binary()) -> tuple().
get_partitions_count(Client,Topic) ->
  Result = brod:get_partitions_count(Client,Topic),
  Result.

-spec get_producer(atom(),binary(),integer()) -> tuple().
get_producer(Client,Topic,Partition) ->
  brod:get_producer(Client,Topic,Partition).

-spec get_consumer(atom(),binary(),integer()) -> tuple().
get_consumer(Client, Topic, Partition) ->
  brod:get_consumer(Client,Topic,Partition).

-spec fetch([{string(),integer()}],binary(),integer(),integer()) -> tuple().
fetch(ConnOrBootstrap,Topic,Partition,Offset) ->
  brod:fetch(ConnOrBootstrap,Topic,Partition,Offset).

-spec list_groups({string(),integer()},list()) -> tuple().
list_groups(Endpoint,Opts) ->
  brod:list_groups(Endpoint,Opts).

-spec list_all_groups([{string(),integer()}], list()) -> list().
list_all_groups(Endpoint,Opts) ->
  brod:list_all_groups(Endpoint,Opts).
