-module(maui).

-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,start_link/5,terminate/2]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state, {channel,connection,consumer_count,consumer_tag,exchange,queue,message_count,routing_key}).

-define(HOST, "127.0.0.1").
-define(PASSWORD, <<"guest">>).
-define(PORT, 5672).
-define(SERVER, ?MODULE).
-define(SSL, none).
-define(TIMEOUT, 7_000).
-define(USERNAME, <<"guest">>).

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

timeout_millseconds() -> 5500.

amqp_params() ->
  #amqp_params_network{
     connection_timeout = ?TIMEOUT,
     host = ?HOST,
     password = ?PASSWORD,
     port = ?PORT,
     ssl_options = ?SSL,
     username = ?USERNAME
  }.

start_link(Exchange,Queue,Type,RoutingKey,ConsumerName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Exchange,Queue,Type,RoutingKey,ConsumerName], []).

init([Exchange,Queue,Type,RoutingKey,ConsumerName]) ->
  {ok, Connection} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = binary(Exchange), type = binary(Type)}),
  QueueDeclare = #'queue.declare'{queue = binary(Queue), exclusive = false},
  #'queue.declare_ok'{queue = Queue, message_count = MessageCount, consumer_count = ConsumerCount} = amqp_channel:call(Channel, QueueDeclare),
  #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = binary(Exchange), routing_key = binary(RoutingKey)}),
  BasicConsume = #'basic.consume'{queue = Queue, consumer_tag = binary(ConsumerName), no_local = false, no_ack = true, exclusive = false, nowait = false},
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:subscribe(Channel, BasicConsume, self()),
  receive
    #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
  end,
  {ok, #state{
          channel=Channel,
          connection=Connection,
          consumer_count=ConsumerCount,
          consumer_tag=ConsumerTag,
          exchange=binary(Exchange),
          message_count=MessageCount,
          queue=Queue,
          routing_key=binary(RoutingKey)
         },timeout_millseconds()}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  io:format(" [x] Read messages in Live'~n"),
  {noreply, State, timeout_millseconds()}.

terminate(_Reason, #state{
                      channel=Channel,
                      connection=Connection,
                      consumer_count=_ConsumerCount,
                      consumer_tag=ConsumerTag,
                      exchange=_Exchange,
                      message_count=_MessageCount,
                      queue=_Queue,
                      routing_key=_RoutingKey
                     }) ->
  amqp_channel:call(Channel,#'basic.cancel'{consumer_tag = ConsumerTag}),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% loop(Channel) ->
%   receive
%     exit -> ok ;
%     {#'basic.deliver'{delivery_tag = Tag, exchange = ?EVENTS_EXCHANGE}, #'amqp_msg'{payload = Payload}} ->
%       io:format("Received message: ~p~n", [binary_to_list(Payload)]),
%       amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
%       loop(Channel);
%     _Other ->
%       io:format("Received other message xxxx: ~p~n", [_Other]),
%       loop(Channel)
%   end.
