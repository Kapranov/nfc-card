-module(makalapa).

-export([amqp_params/0,consumer_pid/3,handle_message/0,read_messages/2,send_message/3,setup_consumer/2,start/3]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-define(SERVER, ?MODULE).
-define(EVENTS_CONSUMER, <<"bisque">>).
-define(EVENTS_EXCHANGE, <<"lahaina">>).
-define(EVENTS_QUEUES, <<"aloha_queue">>).
-define(EVENTS_TYPES, <<"fanout">>).

log(Key,Value) -> io:format("~p: ~p~n",[Key,Value]).

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

amqp_params() ->
   #amqp_params_network{
      username = <<"guest">>,
      password = <<"guest">>,
      port = 5672,
      host = "127.0.0.1",
      connection_timeout = 7000,
      ssl_options = none
   }.

consumer_pid(Channel, Queue, ConsumerName) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(fun() ->
                  BasicConsume = #'basic.consume'{queue = binary(Queue), consumer_tag = binary(ConsumerName), no_local = false, no_ack = true, exclusive = false, nowait = false},
                  #'basic.consume_ok'{consumer_tag = _ConsumerTag} = amqp_channel:subscribe(Channel, BasicConsume, self())
             end),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> ok;
    #'basic.consume_ok'{consumer_tag = _ConsumerTag} -> ok
  end,
  {ok, Pid}.

handle_message() ->
  receive
    {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=_RoutingKey}, Content} ->
      #amqp_msg{payload = Payload} = Content,
      io:format("Received message: ~p~n", [binary_to_list(Payload)])
  end.

read_messages(Queue, Timeouts) ->
  receive
    {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=_RoutingKey}, Content} ->
      #amqp_msg{payload = Payload} = Content,
      io:format("Received messages: ~p~n", [binary_to_list(Payload)]),
      read_messages(binary(Queue), 0);
    Any ->
      io:format("received unexpected Any: ~p~n", [Any]),
      read_messages(binary(Queue), 0)
    after 1000 ->
      case Timeouts of
        0 ->
          Timeouts2 = Timeouts + 1,
          read_messages(binary(Queue), Timeouts2);
        5 ->
          io:format("~n"),
          io:format("Message timeout exceeded ~n");
        _ ->
          Timeouts2 = Timeouts + 1,
          io:format("."),
          read_messages(binary(Queue), Timeouts2)
      end
  end.

send_message(Queue, Exchange, Msg) ->
  {ok, Conn} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  amqp_channel:cast(Channel, #'basic.publish'{exchange = binary(Exchange), routing_key = binary(Queue)}, #amqp_msg{payload = binary(Msg)}),
  amqp_channel:close(Channel),
  amqp_connection:close(Conn),
  ok.

setup_consumer(Queue, ConsumerName) ->
  {ok, Conn} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  BasicConsume = #'basic.consume'{queue = binary(Queue), consumer_tag = binary(ConsumerName), no_local = false, no_ack = true, exclusive = false, nowait = false},
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:subscribe(Channel, BasicConsume, self()),
  receive
    #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
  end,
  log(read_messages,"start"),
  Msg = read_messages(binary(Queue), 1),
  io:format("Msg: ~p~n", [Msg]),
  log(read_messages,"finish"),
  BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag},
  #'basic.cancel_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, BasicCancel),
  amqp_channel:close(Channel),
  amqp_connection:close(Conn).

start(Queue, Exchange, Type) ->
  {ok, Conn} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  amqp_channel:call(Channel, #'queue.declare'{queue = binary(Queue)}),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = binary(Exchange), type = binary(Type)}),
  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = binary(Queue)}),
  #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = binary(Queue), exchange = binary(Exchange)}),
  amqp_channel:close(Channel),
  amqp_connection:close(Conn),
  ok.
