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

-spec binary(atom() | list() | binary()) -> binary().
binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

-spec timeout_millseconds() -> non_neg_integer().
timeout_millseconds() -> 5_500.

-spec uuid() -> binary().
uuid() ->
  {A,B,C}=erlang:time(),
  <<A:32,B:32,C:32>>.

-spec ref_to_string() -> bitstring().
ref_to_string() ->
  Idx=make_ref(),
  ListIdx=ref_to_list(Idx),
  list_to_bitstring(ListIdx).

-spec amqp_params() -> #amqp_params_network{
                          connection_timeout :: non_neg_integer(),
                          host :: string(),
                          password :: string(),
                          port :: non_neg_integer(),
                          ssl_options :: atom(),
                          username :: string()
                         }.
amqp_params() ->
  #amqp_params_network{
     connection_timeout=?TIMEOUT,
     host=?HOST,
     password=?PASSWORD,
     port=?PORT,
     ssl_options=?SSL,
     username=?USERNAME
  }.

-spec start_link(
        Queue::string(),
        ConsumerTag::string()) -> {ok,pid()} |
                                  ignore |
                                  {error,{already_started,pid()}}.
start_link(Queue,ConsumerTag)
    when is_binary(Queue);is_binary(ConsumerTag) ->
      ?INFO("Starting: ~p ~p",[Queue,ConsumerTag]),
      {ok, Pid}=gen_server:start_link({local,?SERVER},?SERVER,[Queue,ConsumerTag],[]),
      io:format("Server started with Pid: ~p~n",[Pid]).

-spec stop() -> {reply,atom(),map()} |
                {reply,atom(),map(),non_neg_integer()} |
                no_return() |
                {noreply,map(),non_neg_integer()} |
                {stop,atom(),atom(),map()} |
                {stop,atom(),map()}.
stop() -> gen_server:call(?SERVER,stop,infinity).

-spec off() -> no_return().
off() ->
  init:stop(),
  halt().

-spec fetch() -> {reply,atom(),map()} |
                 {reply,atom(),map(),non_neg_integer()} |
                 no_return() | {noreply,map(),non_neg_integer()} |
                 {stop,atom(),atom(),map()} |
                 {stop,atom(),map()}.
fetch() -> gen_server:call(?SERVER,fetch).

-spec init(list()) -> {ok,map()} |
                      {ok,map(),non_neg_integer} |
                      ignore |
                      {stop,atom()}.
init([Queue,ConsumerTag]) ->
  ?DBG("~nQueue:       ~p" "~nConsumerTag: ~p" "~n",[Queue,ConsumerTag]),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  {ok,#maui_client{channel=Channel,connection=Connection,consumer_tag=ConsumerTag,message_id=0+1,queue=Queue}}.

-spec handle_call(atom(),atom(),map()) -> {reply,atom(),map()} |
                                          {reply,atom(),map(),non_neg_integer()} |
                                          no_return() |
                                          {noreply,map(),non_neg_integer()} |
                                          {stop,atom(),atom(),map()} |
                                          {stop,atom(),map()}.
handle_call(stop,_From,State) ->
  ?DBG("Stop: ~p",[State]),
  {stop,normal,ok,State};
handle_call(fetch,_From,State) ->
  ?DBG("Fetch: ~p",[State]),
  BasicConsume=#'basic.consume'{queue=State#maui_client.queue,consumer_tag=State#maui_client.consumer_tag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=Tag}=amqp_channel:subscribe(State#maui_client.channel,BasicConsume,self()),
  io:format("Got subscription notification...~p~n",[Tag]),
  retrieve(State#maui_client.channel),
  BasicCancel=#'basic.cancel'{consumer_tag=Tag},
  #'basic.cancel_ok'{consumer_tag=Tag}=amqp_channel:call(State#maui_client.channel,BasicCancel),
  {reply,State,State};
handle_call(_Request,_From,State) ->
  {reply,ok,State}.

-spec handle_cast(tuple(),map()) -> no_return() |
                                    {noreply,map(),non_neg_integer()} |
                                    {stop,atom(),map()}.
handle_cast(_Msg,State) -> {noreply,State}.

-spec handle_info(atom(),map()) -> no_return() |
                                   {noreply,map(),non_neg_integer()} |
                                   {stop,atom(),map()}.
handle_info(#'basic.cancel_ok'{},State) ->
  ?DBG("ConsumerTag Cancel: ~p", [State#maui_client.consumer_tag]),
  {noreply,State};
handle_info(timeout,State) ->
  ?DBG("Timeout: ~p",[State]),
  {noreply,State,timeout_millseconds()};
handle_info(Info,State) ->
  ?DBG("Handle Info noreply: ~p, ~p",[Info,State]),
  {noreply,State}.

-spec terminate(any(),map()) -> ok.
terminate(_Reason,#maui_client{connection=Connection,channel=Channel}) ->
  ?DBG("Close Channel/Connection: ~p, ~p",[Connection,Channel]),
  error_logger:info_msg("closing channel (~p): ~p~n",[?MODULE, channel]),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok;
terminate(Reason,State) ->
  ?DBG("Terminate: ~p, ~p",[Reason,State]),
  error_logger:info_msg("closing channel (~p): ~p~n",[?MODULE,State#maui_client.channel]),
  ok.

-spec code_change(any(),map(),any()) -> {ok,map()}.
code_change(OldVsn,State,Extra) ->
  ?DBG("Code Change: ~p, ~p, ~p",[OldVsn,State,Extra]),
  {ok,State}.

-spec retrieve(pid()) -> nil.
retrieve(Channel) ->
  receive
    {#'basic.deliver'{consumer_tag=_ConsumerTag,delivery_tag=_DeliveryTag,exchange=_Exchange,routing_key=_RoutingKey},#'amqp_msg'{payload=Payload,props=_Props}} ->
      io:format(" [x] JsonBinary received message: ~p~n",[Payload]),
      Message = "Basic return\nPayload: ~p~n",
      io:format(Message,[jsx:decode(Payload)]),
      retrieve(Channel);
    _Others ->
      retrieve(Channel)
    after 400 ->
      io:format("~n"),
      io:format("Time out in seconds has been reached ~n~n")
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec basic_test() -> ok.
basic_test() ->
  ConsumerTag1 = <<"test_customer_tag1">>,
  ConsumerTag2 = <<"test_customer_tag2">>,
  ConsumerTag3 = <<"test_customer_tag3">>,
  Daddy        = self(),
  Durable      = true,
  Msg1         = jsx:encode(#{name => <<"Taumua">>,age => 19,city => <<"Kula">>}),
  Msg2         = jsx:encode(#{name => <<"Kahele">>,age => 42,city => <<"Hilo">>}),
  Msg3         = jsx:encode(#{name => <<"Morikawa">>,age => 56,city => <<"Kokua">>}),
  Queue1       = <<"test_queue1">>,
  Queue2       = <<"test_queue2">>,
  Queue3       = <<"test_queue3">>,
  RK1          = <<"test_routing_key1">>,
  RK2          = <<"test_routing_key2">>,
  RK3          = <<"test_routing_key3">>,
  Type         = <<"direct">>,
  Exchange1    = <<"test_exchange1">>,
  Exchange2    = <<"test_exchange2">>,
  Exchange3    = <<"test_exchange3">>,
  Exchanges1   = [{Exchange1,Type,Durable}],
  Exchanges2   = [{Exchange2,Type,Durable}],
  Exchanges3   = [{Exchange3,Type,Durable}],
  ok           = declare_exchanges(Exchanges1,Queue1,RK1),
  ok           = declare_exchanges(Exchanges1,Queue2,RK1),
  ok           = declare_exchanges(Exchanges1,Queue3,RK1),
  ok           = declare_exchanges(Exchanges1,Queue1,RK2),
  ok           = declare_exchanges(Exchanges1,Queue2,RK2),
  ok           = declare_exchanges(Exchanges1,Queue3,RK2),
  ok           = declare_exchanges(Exchanges1,Queue1,RK3),
  ok           = declare_exchanges(Exchanges1,Queue2,RK3),
  ok           = declare_exchanges(Exchanges1,Queue3,RK3),
  ok           = declare_exchanges(Exchanges2,Queue1,RK1),
  ok           = declare_exchanges(Exchanges2,Queue2,RK1),
  ok           = declare_exchanges(Exchanges2,Queue3,RK1),
  ok           = declare_exchanges(Exchanges2,Queue1,RK2),
  ok           = declare_exchanges(Exchanges2,Queue2,RK2),
  ok           = declare_exchanges(Exchanges2,Queue3,RK2),
  ok           = declare_exchanges(Exchanges2,Queue1,RK3),
  ok           = declare_exchanges(Exchanges2,Queue2,RK3),
  ok           = declare_exchanges(Exchanges2,Queue3,RK3),
  ok           = declare_exchanges(Exchanges3,Queue1,RK1),
  ok           = declare_exchanges(Exchanges3,Queue2,RK1),
  ok           = declare_exchanges(Exchanges3,Queue3,RK1),
  ok           = declare_exchanges(Exchanges3,Queue1,RK2),
  ok           = declare_exchanges(Exchanges3,Queue2,RK2),
  ok           = declare_exchanges(Exchanges3,Queue3,RK2),
  ok           = declare_exchanges(Exchanges3,Queue1,RK3),
  ok           = declare_exchanges(Exchanges3,Queue2,RK3),
  ok           = declare_exchanges(Exchanges3,Queue3,RK3),
  ok           = declare_publish(Exchanges1,Msg1,RK1),
  ok           = declare_publish(Exchanges1,Msg2,RK1),
  ok           = declare_publish(Exchanges1,Msg3,RK1),
  ok           = declare_publish(Exchanges1,Msg1,RK2),
  ok           = declare_publish(Exchanges1,Msg2,RK2),
  ok           = declare_publish(Exchanges1,Msg3,RK2),
  ok           = declare_publish(Exchanges1,Msg1,RK3),
  ok           = declare_publish(Exchanges1,Msg2,RK3),
  ok           = declare_publish(Exchanges1,Msg3,RK3),
  ok           = declare_publish(Exchanges2,Msg1,RK1),
  ok           = declare_publish(Exchanges2,Msg2,RK1),
  ok           = declare_publish(Exchanges2,Msg3,RK1),
  ok           = declare_publish(Exchanges2,Msg1,RK2),
  ok           = declare_publish(Exchanges2,Msg2,RK2),
  ok           = declare_publish(Exchanges2,Msg3,RK2),
  ok           = declare_publish(Exchanges2,Msg1,RK3),
  ok           = declare_publish(Exchanges2,Msg2,RK3),
  ok           = declare_publish(Exchanges2,Msg3,RK3),
  ok           = declare_publish(Exchanges3,Msg1,RK1),
  ok           = declare_publish(Exchanges3,Msg2,RK1),
  ok           = declare_publish(Exchanges3,Msg3,RK1),
  ok           = declare_publish(Exchanges3,Msg1,RK2),
  ok           = declare_publish(Exchanges3,Msg2,RK2),
  ok           = declare_publish(Exchanges3,Msg3,RK2),
  ok           = declare_publish(Exchanges3,Msg1,RK3),
  ok           = declare_publish(Exchanges3,Msg2,RK3),
  ok           = declare_publish(Exchanges3,Msg3,RK3),
  ok           = wait_for_connections(100),
  ok           = declare_customer(Daddy,Queue1,ConsumerTag1),
  ok           = declare_customer(Daddy,Queue1,ConsumerTag2),
  ok           = declare_customer(Daddy,Queue1,ConsumerTag3),
  ok           = declare_customer(Daddy,Queue2,ConsumerTag1),
  ok           = declare_customer(Daddy,Queue2,ConsumerTag2),
  ok           = declare_customer(Daddy,Queue2,ConsumerTag3),
  ok           = declare_customer(Daddy,Queue3,ConsumerTag1),
  ok           = declare_customer(Daddy,Queue3,ConsumerTag2),
  ok           = declare_customer(Daddy,Queue3,ConsumerTag3),
  ok           = wait_for_connections(1_000),
  [<<131,97,1>>,<<131,97,2>>,<<131,97,3>>,<<131,97,4>>]=basic_dataset(),
  ok.

-spec declare_exchanges(string(),string(),string()) -> ok.
declare_exchanges(Exchanges,Queue,RK)
  when is_list(Exchanges) ,
       is_binary(Queue),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  [#'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{exchange=Name,type=Type,durable=Durable}) || {Name,Type,Durable} <- Exchanges],
  ok = declare_queue(Channel,Exchanges,Queue,RK),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

-spec declare_queue(pid(),string(),string(),string()) -> ok.
declare_queue(Channel,Exchanges,Queue,RK)
  when is_pid(Channel),
       is_list(Exchanges),
       is_binary(Queue),
       is_binary(RK) ->
  QueueDeclare=#'queue.declare'{queue=Queue,exclusive=false,auto_delete=false,durable=true},
  #'queue.declare_ok'{queue=Queue}=amqp_channel:call(Channel,QueueDeclare),
  [#'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=element(1,E),routing_key=RK}) || E <- Exchanges],
  ok.

-spec declare_publish(string(),binary(),string()) -> ok.
declare_publish(Exchanges,Msg,RK)
  when is_list(Exchanges) ,
       is_binary(Msg),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  [Publish]=[#'basic.publish'{exchange=element(1,E),mandatory=true,routing_key=RK} || E <- Exchanges],
  amqp_channel:call(Channel,Publish,#amqp_msg{payload=Msg}),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

-spec declare_customer(pid(),string(),string()) -> ok.
declare_customer(Daddy,Queue,ConsumerTag)
  when is_pid(Daddy),
       is_binary(Queue),
       is_binary(ConsumerTag) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  BasicConsume=#'basic.consume'{queue=Queue,consumer_tag=ConsumerTag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=Tag}=amqp_channel:subscribe(Channel,BasicConsume,Daddy),
  io:format("Got subscription notification...~p~n", [Tag]),
  retrieve(Channel),
  BasicCancel=#'basic.cancel'{consumer_tag=Tag},
  #'basic.cancel_ok'{consumer_tag=Tag}=amqp_channel:call(Channel,BasicCancel),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

-spec basic_dataset() -> [binary()].
basic_dataset() ->
  [term_to_binary(Term) || Term <- [1, 2, 3, 4]].

-spec wait_for_connections(non_neg_integer()) -> ok.
wait_for_connections(Num) ->
  case [] == 0 of
    true  -> timer:sleep(Num),
             wait_for_connections(Num);
    false -> ok
  end.
-else.
-endif.
%%%_* Tests ============================================================
