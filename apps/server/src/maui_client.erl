-module(maui_client).

-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,start_link/2,stop/0,terminate/2,fetch/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state,{channel,connection,consumer_tag,queue}).

-define(HOST,"127.0.0.1").
-define(PASSWORD,<<"guest">>).
-define(PORT,5672).
-define(SERVER,?MODULE).
-define(SSL,none).
-define(TIMEOUT,7_000).
-define(USERNAME,<<"guest">>).

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

start_link(Queue,ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[binary(Queue),binary(ConsumerTag)],[]).

stop() -> gen_server:call(?SERVER,stop,infinity).

fetch() ->
  gen_server:call(?SERVER,fetch).

init([Queue,ConsumerTag]) ->
  process_flag(trap_exit,true),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  {ok,#state{connection=Connection,channel=Channel,consumer_tag=ConsumerTag,queue=Queue},timeout_millseconds()}.

handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(fetch,_From,State) ->
  BasicConsume=#'basic.consume'{queue=State#state.queue,consumer_tag=State#state.consumer_tag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=ConsumerTag}=amqp_channel:subscribe(State#state.channel,BasicConsume,self()),
  retrieve(State#state.channel),
  BasicCancel=#'basic.cancel'{consumer_tag=ConsumerTag},
  #'basic.cancel_ok'{consumer_tag=ConsumerTag}=amqp_channel:call(State#state.channel,BasicCancel),
  {reply, State, State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info({#'basic.consume'{},_Pid},State) -> {noreply,State};
handle_info(#'basic.consume_ok'{},State) -> {noreply,State};
handle_info(#'basic.cancel'{},State) -> {noreply,State};
handle_info(#'basic.cancel_ok'{},State) -> {noreply,State};
handle_info(fetch,State) -> {noreply,State};
handle_info(timeout,State) ->
  io:format("Connected by Queue: ~p~n", [State#state.queue]),
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

retrieve(Channel) ->
  receive
    {#'basic.deliver'{delivery_tag=DeliveryTag},Content} ->
      #'amqp_msg'{payload=Payload}=Content,
      io:format("Received message: ~p~n",[binary_to_list(Payload)]),
      retrieve(Channel);
    _Others ->
      retrieve(Channel)
    after 1000 ->
      io:format("~n"),
      io:format("Message timeout exceeded ~n")
  end.
