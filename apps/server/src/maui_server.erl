-module(maui_server).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([code_change/3
        ,generate_msg_id/0
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,publish/1
        ,start_link/5
        ,stop/0
        ,terminate/2
        ,cleanup/0
        ]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").
-include("./apps/server/include/base.hrl").

-spec generate_age() -> non_neg_integer().
generate_age() -> fakerl:random(0,99).
-spec generate_city() -> string().
generate_city() -> fakerl:random_city().
-spec generate_name() -> string().
generate_name() -> fakerl:random_name().

-spec binary(atom() | list() | binary()) -> binary().
binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

-spec generate_msg_id() -> string().
generate_msg_id() ->
  list_to_binary(uuidx:uuid_v4()).

-spec time_since_epoch() -> non_neg_integer().
time_since_epoch() ->
  Now=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  Now*1_000.

-spec timeout_millseconds() -> non_neg_integer().
timeout_millseconds() -> 5_500.

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
     connection_timeout=?RABBIT_CONNECTION_TIMEOUT,
     host=?RABBIT_HOST,
     password=?RABBIT_PASSWORD,
     port=?RABBIT_PORT,
     ssl_options=?RABBIT_SSL_OPTIONS,
     username=?RABBIT_USERNAME
  }.

-spec start_link(
        Exchange::string(),
        Queue::string(),
        Type::boolean(),
        RK::string(),
        ConsumerTag::string()) -> {ok,pid()} |
                                  atom() |
                                  {error,{already_started,pid()}}.
start_link(Exchange,Queue,Type,RK,ConsumerTag) when is_binary(Exchange);
                                                    is_binary(Queue);
                                                    is_binary(Type);
                                                    is_binary(RK);
                                                    is_binary(ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[Exchange,Queue,Type,RK,ConsumerTag],[]).

-spec stop() -> {reply,atom(),map()} |
                {reply,atom(),map(),non_neg_integer()} |
                no_return() |
                {noreply,map(),non_neg_integer()} |
                {stop,atom(),atom(),map()} |
                {stop,atom(),map()}.
stop() -> gen_server:call(?SERVER,stop,infinity).

cleanup() -> exit(?MODULE, kill).

-spec publish(string()) -> no_return() |
                           {noreply,map(),non_neg_integer()} |
                           {stop,atom(),map()}.
publish(Msg) when is_map(Msg) ->
  gen_server:cast(?SERVER,{publish,jsx:encode(Msg)}).

-spec init(list()) -> {ok,map()} |
                      {ok,map(),non_neg_integer} |
                      ignore |
                      {stop,atom()}.
init([Exchange,Queue,Type,RK,ConsumerTag]) ->
  process_flag(trap_exit,true),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{arguments=[{"main-exchange",longstr,Exchange}],exchange=binary(Exchange),type=binary(Type),durable=true}),
  QueueDeclare=#'queue.declare'{arguments=[{"x-ha-policy",longstr,"nodes"},{"x-ha-nodes",array,[{longstr,"lugatex@yahoo.com"}]}],queue=binary(Queue),exclusive=false,auto_delete=false,durable=true},
  #'queue.declare_ok'{queue=Queue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=binary(Exchange),routing_key=binary(RK)}),
  io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),
  {ok,#maui_server{
         channel=Channel,
         connection=Connection,
         consumer_count=ConsumerCount+1,
         consumer_tag=ConsumerTag,
         exchange=binary(Exchange),
         message_count=MessageCount+1,
         queue=Queue,
         routing_key=binary(RK)
       },timeout_millseconds()}.

-spec handle_call(atom(),atom(),map()) -> {reply,atom(),map()} |
                                          {reply,atom(),map(),non_neg_integer()} |
                                          no_return() |
                                          {noreply,map(),non_neg_integer()} |
                                          {stop,atom(),atom(),map()} |
                                          {stop,atom(),map()}.
handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

-spec handle_cast(tuple(),map()) -> no_return() |
                                    {noreply,map(),non_neg_integer()} |
                                    {stop,atom(),map()}.
handle_cast({publish,Msg},#maui_server{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  Headers=[{<<"company">>,binary,<<"StarTech">>}],
  BasicPublish=#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},
  Props=#'P_basic'{delivery_mode=?RABBIT_PERSISTENT_DELIVERY,
                   content_type=?RABBIT_CONTENT_TYPE,
                   message_id=generate_msg_id(),
                   timestamp=time_since_epoch(),
                   headers=Headers
                  },
  ok=amqp_channel:cast(Channel,BasicPublish,#'amqp_msg'{props=Props,payload=binary(Msg)}),
  Message = "Published\nPayload: ~p~n",
  io:format(Message,[jsx:decode(Msg)]),
  {noreply,State};
handle_cast(_Msg,State) -> {noreply,State}.

-spec handle_info(atom(),map()) -> no_return() |
                                   {noreply,map(),non_neg_integer()} |
                                   {stop,atom(),map()}.
handle_info(timeout,#maui_server{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  amqp_channel:register_return_handler(Channel,self()),
  amqp_channel:register_confirm_handler(Channel,self()),
  %amqp_channel:call(Channel,#'confirm.select'{nowait=true}),
  Term=#{age=>generate_age(),city=>generate_city(),name=>generate_name()},
  Props=#'P_basic'{delivery_mode=?RABBIT_PERSISTENT_DELIVERY,content_type=?RABBIT_CONTENT_TYPE},
  Msg=#'amqp_msg'{props=Props,payload=jsx:encode(Term)},
  amqp_channel:cast(Channel,#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},Msg),
  io:format("~s~n",[jsx:encode(Term)]),
  {noreply,State,timeout_millseconds()};
handle_info(shutdown,State) ->
  {stop,normal,State};
handle_info({'DOWN', _MRef, process, _Pid, _Info}, State) ->
  {noreply, State};
handle_info(Info,State) ->
  io:format("unexpected info: ~p~n",[Info]),
  {noreply,State}.

-spec terminate(any(),map()) -> ok.
terminate(_Reason,#maui_server{connection=Connection,channel=Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok;
terminate(_Reason,_State) -> ok.

-spec code_change(any(),map(),any()) -> {ok,map()}.
code_change(_OldVsn,State,_Extra) -> {ok,State}.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec basic_test() -> ok.
basic_test() ->
  Durable      = true,
  Msg1         = jsx:encode(?RABBIT_TEST_PAYLOAD1),
  Msg2         = jsx:encode(?RABBIT_TEST_PAYLOAD2),
  Msg3         = jsx:encode(?RABBIT_TEST_PAYLOAD3),
  Queue1       = ?RABBIT_TEST_QUEUE1,
  Queue2       = ?RABBIT_TEST_QUEUE2,
  Queue3       = ?RABBIT_TEST_QUEUE3,
  RK1          = ?RABBIT_TEST_ROUTING_KEY1,
  RK2          = ?RABBIT_TEST_ROUTING_KEY2,
  RK3          = ?RABBIT_TEST_ROUTING_KEY3,
  Type         = ?RABBIT_TEST_TYPE,
  Exchange1    = ?RABBIT_TEST_EXCHANGE1,
  Exchange2    = ?RABBIT_TEST_EXCHANGE2,
  Exchange3    = ?RABBIT_TEST_EXCHANGE3,
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
  [<<131,97,1>>,<<131,97,2>>,<<131,97,3>>,<<131,97,4>>]=basic_dataset(),
  ok.

-spec declare_exchanges(string(),string(),string()) -> ok.
declare_exchanges(Exchanges,Queue,RK)
  when is_list(Exchanges) ,
       is_binary(Queue),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  amqp_channel:register_return_handler(Channel,self()),
  amqp_channel:register_confirm_handler(Channel,self()),
  amqp_channel:call(Channel,#'confirm.select'{nowait=true}),
  [#'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{arguments=[{"main-exchange",longstr,Name}],exchange=Name,type=Type,durable=Durable}) || {Name,Type,Durable} <- Exchanges],
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
  QueueDeclare=#'queue.declare'{arguments=[{"x-ha-policy",longstr,"nodes"},{"x-ha-nodes",array,[{longstr,"lugatex@yahoo.com"}]}],queue=Queue,exclusive=false,auto_delete=false,durable=true},
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
