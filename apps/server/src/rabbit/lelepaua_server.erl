-module(lelepaua_server).

-behaviour(gen_server).

-export([start_link/0, publish/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EVENTS_CONSUMER, <<"bisque">>).
-define(EVENTS_EXCHANGE, <<"lahaina">>).
-define(EVENTS_MESSAGES, <<"aloha_queue">>).
-define(EVENTS_PAYLOADS, <<"Aloha, NFC-card!">>).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state, {connection, channel}).

timeout_millseconds() -> 5500.

publish(Message) ->
  gen_server:call(?MODULE, {publish, Message}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Connection} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  {ok, #state{connection = Connection, channel = Channel}, timeout_millseconds()}.

handle_call({publish, Message}, _From, State = #state{channel = Channel}) ->
  amqp_channel:cast(Channel, #'basic.publish'{exchange = ?EVENTS_EXCHANGE, routing_key = ?EVENTS_MESSAGES}, #amqp_msg{payload = Message}),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State = #state{channel = Channel}) ->
  amqp_channel:call(Channel, #'exchange.declare'{exchange = ?EVENTS_EXCHANGE, type = <<"direct">>}),
  amqp_channel:call(Channel, #'queue.declare'{queue = ?EVENTS_MESSAGES}),
  amqp_channel:cast(Channel, #'basic.publish'{exchange = ?EVENTS_EXCHANGE, routing_key = ?EVENTS_MESSAGES}, #amqp_msg{payload = ?EVENTS_PAYLOADS}),
  io:format(" [x] Sent 'Aloha, NFC-card!'~n"),
  {noreply, State, timeout_millseconds()}.

terminate(_Reason, #state{connection = Connection,channel = Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
