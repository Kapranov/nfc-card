-module(kailua).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-export([start/0, amqp_params/0]).

-define(EVENTS_EXCHANGE, <<"lahaina">>).
-define(EVENTS_MESSAGES, <<"aloha_queue">>).
-define(EVENTS_PAYLOADS, <<"Aloha, NFC-card!">>).
-define(EVENTS_NEXT_PAYLOADS, <<"Kokua Line: How do we get discount on sewer fee hike?">>).
-define(EVENTS_TYPES, <<"direct">>).

start() ->
    amqp_lifecycle().

amqp_lifecycle() ->
  {ok, Connection} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  X = ?EVENTS_EXCHANGE,
  BindKey = <<"#">>,
  QueueDeclare = #'queue.declare'{queue = ?EVENTS_MESSAGES, exclusive = false},
  #'queue.declare_ok'{queue = Queue, message_count = MessageCount, consumer_count = ConsumerCount} = amqp_channel:call(Channel, QueueDeclare),
  log(queue,Queue),
  log(message_count,MessageCount),
  log(consumer_count,ConsumerCount),
  ExchangeDeclare = #'exchange.declare'{exchange = X, type = ?EVENTS_TYPES},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
  QueueBind = #'queue.bind'{queue = Queue, exchange = X, routing_key = BindKey},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
  log(send_message,"start"),
  RoutingKey = ?EVENTS_MESSAGES,
  Payload = ?EVENTS_PAYLOADS,
  NextPayload = ?EVENTS_NEXT_PAYLOADS,
  send_message(Channel, X, RoutingKey, Payload),
  send_message(Channel, X, RoutingKey, NextPayload),
  log(setup_consumer,"start"),
  setup_consumer(Channel, Queue),
  log(setup_consumer,"finished"),
  log(channel_close,"start"),
  ok = amqp_channel:close(Channel),
  log(connection_close,"start"),
  ok = amqp_connection:close(Connection),
  log(connection_close,"Demo Completed!"),
  ok.

send_message(Channel, X, RoutingKey, Payload) ->
  log(send_message,"basic.publish setup"),
  BasicPublish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
  log(send_message,"amqp_channel:cast"),
  ok = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}).

setup_consumer(Channel, Queue) ->
  log(setup_consumer,"basic.consume"),
  BasicConsume = #'basic.consume'{queue = Queue, consumer_tag = <<"">>, no_ack = true},
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:subscribe(Channel, BasicConsume, self()),
  log(setup_consumer,"basic.consume_ok start receive"),
  receive
    #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
  end,
  log(setup_consumer,"basic.consume_ok finished"),
  log(read_messages,"start"),
  Msg = read_messages(0),
  io:format("Msg: ~p~n", [Msg]),
  log(read_messages,"finish"),
  log(basic_cancel,"start"),
  BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag},
  #'basic.cancel_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel,BasicCancel).

read_messages(Timeouts) ->
  receive
    {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=RoutingKey}, Content} ->
      log(read_messages,"basic.deliver"),
      io:format("RoutingKey received: ~p~n", [RoutingKey]),
      #amqp_msg{payload = Payload} = Content,
      io:format("Payload received: ~p~n", [Payload]),
      read_messages(0);
    Any ->
      io:format("received unexpected Any: ~p~n", [Any]),
      read_messages(0)
  after 1000 ->
    case Timeouts of
      0 ->
        Timeouts2 = Timeouts + 1,
        read_messages(Timeouts2);
      5 ->
        io:format("~n"),
        io:format("Message timeout exceeded ~n");
      _ ->
        Timeouts2 = Timeouts + 1,
        io:format("."),
        read_messages(Timeouts2)
    end
  end.

log(Key,Value) ->
    io:format("~p: ~p~n",[Key,Value]).

amqp_params() ->
   #amqp_params_network{
      username = <<"guest">>,
      password = <<"guest">>,
      port = 5672,
      host = "127.0.0.1",
      connection_timeout = 7000,
      ssl_options = none
   }.
