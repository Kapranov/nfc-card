-module(waialua_publish).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([publish/2]).

-spec publish(string(),string()) -> tuple().
publish(Key,Content) ->
  Topic = <<"test-topic">>,
  Partition=0,
  ok=brod:start_producer(client1,Topic,_ProducerConfig=[]),
  Msg=iolist_to_binary(jiffy:encode(#{rest => Content})),
  {ok,_FirstOffset}=brod:produce_sync_offset(client1,Topic,Partition,Key,Msg).
