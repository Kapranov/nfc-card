-module(maui).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state, {channel, connection, queue, consumer_tag}).

-define(SERVER, ?MODULE).
-define(EVENTS_CONSUMER, <<"bisque">>).
-define(EVENTS_EXCHANGE, <<"lahaina">>).
-define(EVENTS_QUEUES, <<"aloha_queue">>).
-define(EVENTS_TYPES, <<"topic">>).

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

timeout_millseconds() -> 5500.

amqp_params() ->
  #amqp_params_network{
     username = <<"guest">>,
     password = <<"guest">>,
     port = 5672,
     host = "127.0.0.1",
     connection_timeout = 7000,
     ssl_options = none
  }.

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([]) ->
  {ok, Connection} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = ?EVENTS_EXCHANGE, type = ?EVENTS_TYPES}),
  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = ?EVENTS_QUEUES}),
  #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = binary(Queue), exchange = ?EVENTS_EXCHANGE}),
  {ok, #state{channel = Channel, queue = Queue}, timeout_millseconds()}.

% init(_Args) ->
%   {ok, Conn} = amqp_connection:start(amqp_params()),
%   {ok, Channel} = amqp_connection:open_channel(Conn),
%   #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = ?EVENTS_EXCHANGE, type = <<"fanout">>}),
%   #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = ?EVENTS_MESSAGES, exclusive = false}),
%   #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{exchange = ?EVENTS_EXCHANGE, queue = Queue, routing_key = ?EVENTS_MESSAGES}),
%   %#'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, #'basic.consume'{queue = Queue}),
%   #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue, no_ack = true}, self()),
%   receive
%     #'basic.consume_ok'{} -> ok
%   end,
%   loop(Channel),
%   {ok, #state{queue = Queue, channel = Channel, connection = Conn, consumer_tag = Tag}}.
%
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  io:format(" [x] Read messages in Live'~n"),
  {noreply, State, timeout_millseconds()}.

terminate(_Reason, #state{channel = Channel, consumer_tag = Tag, connection = Conn}) ->
  amqp_channel:call(Channel,#'basic.cancel'{consumer_tag = Tag}),
  amqp_connection:close(Conn),
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
