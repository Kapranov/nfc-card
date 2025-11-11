-module(maui_server).

-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,publish/1,start_link/5,stop/0,terminate/2]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state,{channel,connection,consumer_count,consumer_tag,exchange,queue,message_count,routing_key}).

-define(HOST,"127.0.0.1").
-define(PASSWORD,<<"guest">>).
-define(PORT,5672).
-define(SERVER,?MODULE).
-define(SSL,none).
-define(TIMEOUT,7_000).
-define(USERNAME,<<"guest">>).
-define(MESSAGE,<<"State leaders support alternate Mauna Kea sites as option for Thirty Meter Telescope">>).

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

timeout_millseconds() -> 5_500.

amqp_params() ->
  #amqp_params_network{
     connection_timeout=?TIMEOUT,
     host=?HOST,
     password=?PASSWORD,
     port=?PORT,
     ssl_options=?SSL,
     username=?USERNAME
  }.

start_link(Exchange,Queue,Type,RoutingKey,ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[Exchange,Queue,Type,RoutingKey,ConsumerTag],[]).

stop() -> gen_server:call(?SERVER,stop,infinity).

publish(Message) -> gen_server:cast(?SERVER,{publish,Message}).

init([Exchange,Queue,Type,RoutingKey,ConsumerTag]) ->
  process_flag(trap_exit, true),
  {ok,Connection} = amqp_connection:start(amqp_params()),
  {ok,Channel} = amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel,#'exchange.declare'{exchange=binary(Exchange),type=binary(Type)}),
  QueueDeclare=#'queue.declare'{queue=binary(Queue),exclusive=false,auto_delete=false,durable=false},
  #'queue.declare_ok'{queue=Queue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=binary(Exchange),routing_key=binary(RoutingKey)}),
  io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),
  {ok, #state{
          channel=Channel,
          connection=Connection,
          consumer_count=ConsumerCount+1,
          consumer_tag=ConsumerTag,
          exchange=binary(Exchange),
          message_count=MessageCount+1,
          queue=Queue,
          routing_key=binary(RoutingKey)
        },timeout_millseconds()}.

handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast({publish,Message},State) ->
  BasicPublish=#'basic.publish'{exchange=State#state.exchange,routing_key=State#state.routing_key},
  Props = #'P_basic'{delivery_mode = 2},
  ok=amqp_channel:cast(State#state.channel,BasicPublish,#'amqp_msg'{props=Props,payload=binary(Message)}),
  {noreply,State};
handle_cast(_Msg,State) -> {noreply,State}.

handle_info(timeout,State) ->
  Props=#'P_basic'{delivery_mode=2},
  Msg=#'amqp_msg'{props=Props,payload=?MESSAGE},
  amqp_channel:cast(State#state.channel,#'basic.publish'{exchange=State#state.exchange,routing_key=State#state.routing_key},Msg),
  io:format(" [x] HW Star-Bulletin 'An alternate Mauna Kea sites by Andrew Gomes'~n"),
  io:format("Connected by Exchange: ~p~n", [State#state.exchange]),
  {noreply,State,timeout_millseconds()};
handle_info(Info, State) ->
  io:format("unexpected info: ~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, #state{connection=Connection,channel=Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok;
terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.
