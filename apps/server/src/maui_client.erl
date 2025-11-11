-module(maui_client).
-author('Oleg G.Kapranov').

-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,start_link/2,stop/0,terminate/2,fetch/0,uuid/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").
-include("./apps/server/include/base.hrl").

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

timeout_millseconds() -> 5_500.

uuid() ->
  {A, B, C} = erlang:time(),
  <<A:32, B:32, C:32>>.

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
  ?INFO("Starting: ~p ~p", [Queue,ConsumerTag]),
  gen_server:start_link({local,?SERVER},?SERVER,[binary(Queue),binary(ConsumerTag)],[]).

stop() -> gen_server:call(?SERVER,stop,infinity).

fetch() -> gen_server:call(?SERVER,fetch).

init([Queue,ConsumerTag]) ->
  %process_flag(trap_exit,true),
  ?DBG("~nQueue:       ~p" "~nConsumerTag: ~p" "~n",[Queue,ConsumerTag]),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  {ok,#maui_client{connection=Connection,channel=Channel,consumer_tag=ConsumerTag,queue=Queue},timeout_millseconds()}.

handle_call(stop,_From,State) ->
  ?DBG("Stop: ~p", [State]),
  {stop,normal,ok,State};
handle_call(fetch,_From,State) ->
  ?DBG("Fetch: ~p", [State]),
  BasicConsume=#'basic.consume'{queue=State#maui_client.queue,consumer_tag=State#maui_client.consumer_tag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=Tag}=amqp_channel:subscribe(State#maui_client.channel,BasicConsume,self()),
  io:format("Got subscription notification...~p~n", [Tag]),
  retrieve(State#maui_client.channel),
  BasicCancel=#'basic.cancel'{consumer_tag=Tag},
  #'basic.cancel_ok'{consumer_tag=Tag}=amqp_channel:call(State#maui_client.channel,BasicCancel),
  {reply, State, State};
handle_call(_Request,_From,State) ->
  {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(#'basic.cancel_ok'{},State) ->
  ?DBG("ConsumerTag Cancel: ~p", [State#maui_client.consumer_tag]),
  {noreply,State};
handle_info(timeout,State) ->
  ?DBG("Timeout: ~p", [State]),
  {noreply,State,timeout_millseconds()};
handle_info(Info, State) ->
  ?DBG("Handle Info noreply: ~p, ~p", [Info,State]),
  {noreply,State}.

terminate(_Reason,#maui_client{connection=Connection,channel=Channel}) ->
  ?DBG("Close Channel/Connection: ~p, ~p", [Connection,Channel]),
  error_logger:info_msg("closing channel (~p): ~p~n", [?MODULE, channel]),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok;
terminate(Reason,State) ->
  ?DBG("Terminate: ~p, ~p", [Reason,State]),
  error_logger:info_msg("closing channel (~p): ~p~n", [?MODULE, State#maui_client.channel]),
  ok.

code_change(OldVsn,State,Extra) ->
  ?DBG("Code Change: ~p, ~p, ~p", [OldVsn,State,Extra]),
  {ok,State}.

retrieve(Channel) ->
  receive
    {#'basic.deliver'{delivery_tag=_DeliveryTag},Content} ->
      #'amqp_msg'{payload=Payload}=Content,
      io:format(" [x] Received message: ~p~n",[binary_to_list(Payload)]),
      retrieve(Channel);
    _Others ->
      retrieve(Channel)
    after 1000 ->
      io:format("~n"),
      io:format("Time out in seconds has been reached ~n~n")
  end.
