-module(maui_client).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([binary/1
        ,code_change/3
        ,fetch/0
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,off/0
        ,ref_to_string/0
        ,start_link/2
        ,stop/0
        ,terminate/2
        ,uuid/0
        ]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").
-include("./apps/server/include/base.hrl").

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

timeout_millseconds() -> 5_500.

uuid() ->
  {A, B, C} = erlang:time(),
  <<A:32, B:32, C:32>>.

ref_to_string() ->
  Idx = make_ref(),
  ListIdx = ref_to_list(Idx),
  list_to_bitstring(ListIdx).

amqp_params() ->
  #amqp_params_network{
     connection_timeout=?TIMEOUT,
     host=?HOST,
     password=?PASSWORD,
     port=?PORT,
     ssl_options=?SSL,
     username=?USERNAME
  }.

start_link(Queue,ConsumerTag)
    when is_binary(Queue) and is_binary(ConsumerTag) ->
      ?INFO("Starting: ~p ~p", [Queue,ConsumerTag]),
      {ok, Pid} = gen_server:start_link({local,?SERVER},?SERVER,[Queue,ConsumerTag],[]),
      io:format("Server started with Pid: ~p~n", [Pid]).

stop() -> gen_server:call(?SERVER,stop,infinity).

off() ->
  init:stop(),
  halt().

fetch() -> gen_server:call(?SERVER,fetch).

init([Queue,ConsumerTag]) ->
  ?DBG("~nQueue:       ~p" "~nConsumerTag: ~p" "~n",[Queue,ConsumerTag]),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  {ok,#maui_client{channel=Channel,connection=Connection,consumer_tag=ConsumerTag,message_id=0+1,queue=Queue}}.

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
    {#'basic.deliver'{consumer_tag=_ConsumerTag,delivery_tag=_DeliveryTag,exchange=_Exchange,routing_key=_RoutingKey},#'amqp_msg'{payload=Payload,props=_Props}} ->
      io:format(" [x] JsonBinary received message: ~p~n",[Payload]),
      Message = "Basic return\nPayload: ~p~n",
      io:format(Message, [jsx:decode(Payload)]),
      retrieve(Channel);
    _Others ->
      retrieve(Channel)
    after 1000 ->
      io:format("~n"),
      io:format("Time out in seconds has been reached ~n~n")
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  ConsumerTag = <<"test_customer_tag">>,
  Daddy       = self(),
  Durable     = true,
  Msg         = <<"Kokua Line: Is random call from health survey legit?">>,
  Queue       = <<"test_queue">>,
  RK          = <<"test_routing_key">>,
  Type        = <<"direct">>,
  Exchange    = <<"test_exchange">>,
  Exchanges   = [{Exchange,Type,Durable}],
  ok=declare_exchanges(Exchanges,Queue,RK),
  ok=declare_publish(Exchanges,Msg,RK),
  ok=wait_for_connections(100),
  ok=declare_customer(Daddy,Queue,ConsumerTag),
  ok=wait_for_connections(1_000),
  [<<131,97,1>>,<<131,97,2>>,<<131,97,3>>,<<131,97,4>>] = basic_dataset(),
  ok.

declare_exchanges(Exchanges,Queue,RK)
  when is_list(Exchanges) ,
       is_binary(Queue),
       is_binary(RK) ->
  {ok,Connection} = amqp_connection:start(amqp_params()),
  {ok,Channel} = amqp_connection:open_channel(Connection),
  [#'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{exchange=Name,type=Type,durable=Durable}) || {Name,Type,Durable} <- Exchanges],
  ok = declare_queue(Channel,Exchanges,Queue,RK),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

declare_queue(Channel,Exchanges,Queue,RK)
  when is_pid(Channel),
       is_list(Exchanges),
       is_binary(Queue),
       is_binary(RK) ->
  QueueDeclare=#'queue.declare'{queue=Queue,exclusive=false,auto_delete=false,durable=true},
  #'queue.declare_ok'{queue=Queue}=amqp_channel:call(Channel,QueueDeclare),
  [#'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=element(1,E),routing_key=RK}) || E <- Exchanges],
  ok.

declare_publish(Exchanges,Msg,RK)
  when is_list(Exchanges) ,
       is_binary(Msg),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  [Publish] = [#'basic.publish'{exchange=element(1,E),mandatory=true,routing_key=RK} || E <- Exchanges],
  amqp_channel:call(Channel,Publish,#amqp_msg{payload=Msg}),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

declare_customer(Daddy,Queue,ConsumerTag)
  when is_pid(Daddy),
       is_binary(Queue),
       is_binary(ConsumerTag) ->
  {ok,Connection} = amqp_connection:start(amqp_params()),
  {ok,Channel} = amqp_connection:open_channel(Connection),
  BasicConsume=#'basic.consume'{queue=Queue,consumer_tag=ConsumerTag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=Tag}=amqp_channel:subscribe(Channel,BasicConsume,Daddy),
  io:format("Got subscription notification...~p~n", [Tag]),
  retrieve(Channel),
  BasicCancel=#'basic.cancel'{consumer_tag=Tag},
  #'basic.cancel_ok'{consumer_tag=Tag}=amqp_channel:call(Channel,BasicCancel),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

basic_dataset() ->
  [term_to_binary(Term) || Term <- [1, 2, 3, 4]].

wait_for_connections(Num) ->
  case [] == 0 of
    true  -> timer:sleep(Num),
             wait_for_connections(Num);
    false -> ok
  end.
-else.
-endif.
%%%_* Tests ============================================================
