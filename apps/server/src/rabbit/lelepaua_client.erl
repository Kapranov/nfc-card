-module(lelepaua_client).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EVENTS_CONSUMER, <<"bisque">>).
-define(EVENTS_EXCHANGE, <<"lahaina">>).
-define(EVENTS_QUEUES, <<"aloha_queue">>).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state, {channel, connection, consumer_tag, queue}).

timeout_millseconds() -> 0.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Connection} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  BasicConsume = #'basic.consume'{queue = ?EVENTS_QUEUES, consumer_tag = ?EVENTS_CONSUMER, no_ack = true},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = ?EVENTS_QUEUES, exchange = ?EVENTS_EXCHANGE}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:subscribe(Channel, BasicConsume, self()),
  receive
    #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
  end,
  {ok, #state{queue = ?EVENTS_QUEUES, channel = Channel, connection = Connection, consumer_tag = ConsumerTag}, timeout_millseconds()}.

handle_info(timeout, #state{queue = Queue, channel = Channel, connection = _Connection, consumer_tag = _ConsumerTag} = State) ->
  amqp_channel:cast(Channel, #'basic.publish'{exchange = ?EVENTS_EXCHANGE, routing_key = Queue}, #amqp_msg{payload = <<"Hello World!">>}),
  read_messages(0),
  {noreply, State}.
%  case timeout of
%    {#'basic.deliver'{}, #'amqp_msg'{payload = Payload}} ->
%      io:format("Received message: ~p~n", [Payload]),
%      {noreply, State};
%    _ ->
%      {noreply, State}
%  end.

%handle_info({#'basic.deliver'{delivery_tag = _ConsumerTag}, Content}, State) ->
%handle_info({#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=_RoutingKey}, #'amqp_msg'{payload = Msg}}, State) ->
%handle_info({#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=_RoutingKey}, #'amqp_msg'{payload = Msg}}, #state{queue = _Queue, channel = _Channel, connection = _Connection, consumer_tag = _ConsumerTag} = State) ->
%  amqp_channel:cast(Channel, #'basic.publish'{exchange = ?EVENTS_EXCHANGE, routing_key = Queue}, #amqp_msg{payload = <<"Hello World!">>}),
%  io:format(" [x] Sent 'Hello World!'~n"),
%  io:format("Received message: ~p~n", [Msg]),
%  io:format("Received message: ~p~n", [binary_to_list(Payload)]),
%  {noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

terminate(_Reason, #state{connection = Connection,channel = Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

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
          %io:format("~n"),
          %io:format("Message timeout exceeded ~n");
          Timeouts2 = Timeouts + 1,
          io:format("."),
          read_messages(Timeouts2);
        _ ->
          Timeouts2 = Timeouts + 1,
          io:format("."),
          read_messages(Timeouts2)
      end
  end.

log(Key,Value) ->
  io:format("~p: ~p~n",[Key,Value]).

amqp_params() ->
  {ok, Host} = application:get_env(server, rabbit_host),
  {ok, Port} = application:get_env(server, rabbit_port),
  {ok, Ssl} = application:get_env(server, rabbit_ssl_options),
  {ok, Timeout} = application:get_env(server, rabbit_connection_timeout),
   #amqp_params_network{
      connection_timeout = Timeout,
      host = Host,
      password = <<"guest">>,
      port = Port,
      ssl_options = Ssl,
      username = <<"guest">>
   }.
