-module(maui_server).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([ack_message/2
        ,amqp_args/1
        ,amqp_config/0
        ,amqp_params/1
        ,basic_cancel/2
        ,bind/4
        ,code_change/3
        ,consume/2
        ,consumer/0
        ,consumer_init/3
        ,consumer_start/3
        ,delete_exchange/3
        ,delete_queue/3
        ,exchange/3
        ,generate_msg_id/0
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,off/0
        ,publish/1
        ,queue/1
        ,retrieve/0
        ,retrieve/2
        ,start/0
        ,start_link/6
        ,stop/0
        ,terminate/2
        ,time_since_epoch/0
        ,unbind/4
        ]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").
-include("./apps/server/include/maui_server.hrl").

-spec log(atom(),string()) -> string().
log(Key,Value) -> io:format("~p: ~p~n",[Key,Value]).

-spec binary(atom() | list() | binary()) -> binary().
binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

-spec generate_age() -> non_neg_integer().
generate_age() -> fakerl:random(0,99).

-spec generate_city() -> string().
generate_city() -> fakerl:random_city().

-spec generate_name() -> string().
generate_name() -> fakerl:random_name().

-spec generate_msg_id() -> string().
generate_msg_id() ->
  list_to_binary(uuidx:uuid_v4()).

-spec time_since_epoch() -> non_neg_integer().
time_since_epoch() ->
  Now=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  Now*1_000.

-spec timeout_millseconds() -> non_neg_integer().
timeout_millseconds() -> 5_500.

call(Args) -> gen_server:call(?SERVER,Args).
call(Args,Opts) -> gen_server:call(?SERVER,Args,Opts).
cast(Args) -> gen_server:cast(?SERVER,Args).

-spec amqp_config() -> #amqp_params_network{connection_timeout :: non_neg_integer()
                                           ,heartbeat :: non_neg_integer()
                                           ,host :: string()
                                           ,password :: string()
                                           ,port :: non_neg_integer()
                                           ,ssl_options :: atom()
                                           ,username :: string()
                                           ,virtual_host :: string()
                                           }.
amqp_config() ->
  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
  {ok,RabbitHost}=application:get_env(server,rabbit_host),
  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
  {ok,RabbitPort}=application:get_env(server,rabbit_port),
  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
  Config=[{connection_timeout,RabbitConnectionTimeout}
         ,{heartbeat,RabbitHeartbeat}
         ,{host,RabbitHost}
         ,{password,RabbitPassword}
         ,{port,RabbitPort}
         ,{ssl_options,RabbitSSLOptions}
         ,{username,RabbitUsername}
         ,{virtual_host,RabbitVirtualHost}
         ],
  #amqp_params_network{connection_timeout=proplists:get_value(connection_timeout,Config)
                      ,heartbeat=proplists:get_value(heartbeat,Config)
                      ,host=proplists:get_value(host,Config)
                      ,password=proplists:get_value(password,Config)
                      ,port=proplists:get_value(port,Config)
                      ,ssl_options=proplists:get_value(ssl_options,Config)
                      ,username=proplists:get_value(username,Config)
                      ,virtual_host=proplists:get_value(virtual_host,Config)
                      }.

-spec amqp_args(list()) -> #amqp_params_network{connection_timeout :: non_neg_integer()
                                               ,heartbeat :: non_neg_integer()
                                               ,host :: string()
                                               ,password :: string()
                                               ,port :: non_neg_integer()
                                               ,ssl_options :: atom()
                                               ,username :: string()
                                               ,virtual_host :: string()
                                               }.
amqp_args(Config) ->
  #amqp_params_network{connection_timeout=proplists:get_value(connection_timeout,Config)
                      ,heartbeat=proplists:get_value(heartbeat,Config)
                      ,host=proplists:get_value(host,Config)
                      ,password=proplists:get_value(password,Config)
                      ,port=proplists:get_value(port,Config)
                      ,ssl_options=proplists:get_value(ssl_options,Config)
                      ,username=proplists:get_value(username,Config)
                      ,virtual_host=proplists:get_value(virtual_host,Config)
                      }.

-spec amqp_params(map()) -> map().
amqp_params(Args) ->
   amqp_options:parse(Args).

-spec start()-> {ok,pid()} | atom().
start() ->
  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
  {ok,RabbitConsumer}=application:get_env(server,rabbit_consumer1),
  {ok,RabbitExchange}=application:get_env(server,rabbit_exchange1),
  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
  {ok,RabbitHost}=application:get_env(server,rabbit_host),
  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
  {ok,RabbitPort}=application:get_env(server,rabbit_port),
  {ok,RabbitQueue}=application:get_env(server,rabbit_queue1),
  {ok,RabbitRoutingKey}=application:get_env(server,rabbit_routing_key1),
  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
  {ok,RabbitType}=application:get_env(server,rabbit_type),
  {ok,RabbitUsername}=application:get_env(server,rabbit_password),
  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
  Config=[{connection_timeout,RabbitConnectionTimeout}
         ,{heartbeat,RabbitHeartbeat}
         ,{host,RabbitHost}
         ,{password,RabbitPassword}
         ,{port,RabbitPort}
         ,{ssl_options,RabbitSSLOptions}
         ,{username,RabbitUsername}
         ,{virtual_host,RabbitVirtualHost}
         ],
  case maui_server:start_link(Config,RabbitExchange,RabbitQueue,RabbitType,RabbitRoutingKey,RabbitConsumer) of
    {ok,Pid} -> {ok,Pid};
    {error,{already_started,_}} -> ok;
    _error -> ok
  end.

-spec stop() -> {reply,atom(),map()} |
                {reply,atom(),map(),non_neg_integer()} |
                no_return() |
                {noreply,map(),non_neg_integer()} |
                {stop,atom(),atom(),map()} |
                {stop,atom(),map()} |
                atom().
stop() ->
  try
    call(stop,infinity)
  catch
    exit:{noproc, _} -> ok
  end.

-spec off() -> no_return().
off() ->
  init:stop(),
  halt().

-spec exchange(pid(),string(),string()) -> ok.
exchange(Channel,Name,Type) ->
  Declare=#'exchange.declare'{arguments = [{"main-exchange",longstr,Name}]
                             ,auto_delete=false
                             ,durable=true
                             ,exchange=binary(Name)
                             ,internal=false
                             ,nowait=false
                             ,passive=false
                             ,type=binary(Type)
                             },
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,Declare),
  ok.

-spec delete_exchange(pid(),atom(),binary()) -> ok.
delete_exchange(Channel,exchange,Exchange) ->
  ExchangeDelete=#'exchange.delete'{exchange=binary(Exchange),nowait=false},
  #'exchange.delete_ok'{}=amqp_channel:call(Channel,ExchangeDelete),
  ok.

queue(Name) ->
  call({queue,Name}).

-spec delete_queue(pid(),atom(),binary()) -> ok.
delete_queue(Channel,queue,Queue) ->
  QueueDelete=#'queue.delete'{queue=binary(Queue),nowait=false},
  #'queue.delete_ok'{message_count=MessageCount}=amqp_channel:call(Channel,QueueDelete),
  io:format("Message count: ~p~n",[MessageCount]),
  ok.

retrieve() ->
  call(retrieve).

consumer() ->
  cast(consumer).

consume(Channel,Queue) ->
  consumer_start(Channel,Queue,self()).

consumer_start(Channel,Queue,Consumer)
  when is_pid(Channel) and is_pid(Consumer) ->
  proc_lib:start(?SERVER,consumer_init,[Channel,Queue,Consumer],10000).

consumer_init(Channel,Queue,Consumer) ->
  {ok,ConsumerName}=application:get_env(server,rabbit_consumer1),
  BasicConsume=#'basic.consume'{arguments=[]
                               ,consumer_tag =ConsumerName
                               ,exclusive=false
                               ,no_ack=true
                               ,no_local=false
                               ,nowait=false
                               ,queue=binary(Queue)
                               },
  case amqp_channel:subscribe(Channel,BasicConsume,self()) of
    #'basic.consume_ok'{consumer_tag=_Tag} ->
      receive
        #'basic.consume_ok'{consumer_tag=ConsumerTag} ->
          proc_lib:init_ack({ok,self(),ConsumerTag}),
          ChannelRef=erlang:monitor(process,Channel),
          ConsumerRef=erlang:monitor(process,Consumer),
          ConsumerState=#consumer_state{channel=Channel
                                       ,channel_ref=ChannelRef
                                       ,consumer=Consumer
                                       ,consumer_ref=ConsumerRef
                                       ,consumer_tag=ConsumerTag
                                       },
          consumer_loop(ConsumerState);
        Msg ->
          error_logger:error_msg("error consume result:~p~n",[Msg]),
          proc_lib:init_ack({error,consume_error})
      after
        5_000 ->
          proc_lib:init_ack({error, consume_timeout})
      end;
    Result ->
      error_logger:error_msg("error subscribe result:~p~n",[Result]),
      proc_lib:init_ack({error,subscribe_error})
  end.

consumer_loop(#consumer_state{channel=Channel
                             ,channel_ref=ChannelRef
                             ,consumer=Consumer
                             ,consumer_ref=_ConsumerRef
                             ,consumer_tag=ConsumerTag
                             }=ConsumerState) ->
  receive
    {#'basic.deliver'{consumer_tag=ConsumerTag
                     ,delivery_tag=DeliveryTag
                     ,exchange=_Exchange
                     ,redelivered=_Redelivered
                     ,routing_key=RoutingKey
                     },#'amqp_msg'{props=Props,payload=Payload}} ->
      io:format("#1 COMMAN Deliver: ~p - ~s ~n", [DeliveryTag,Payload]),
      #'P_basic'{app_id=_AppId
                ,cluster_id=_ClusterId
                ,content_encoding=ContentEncoding
                ,content_type=ContentType
                ,correlation_id=CorrelationId
                ,delivery_mode=_DeliveryMode
                ,expiration=_Expiration
                ,headers=_Headers
                ,message_id=MessageId
                ,priority=_Priority
                ,reply_to=_ReplyTo
                ,timestamp=TimeStamp
                ,type=_Type
                ,user_id=_UserId
                }=Props,
      Headers=[{content_encoding,ContentEncoding}
              ,{content_type,ContentType}
              ,{correlation_id,CorrelationId}
              ,{message_id,MessageId}
              ,{timestamp,TimeStamp}],
      Consumer ! {deliver,RoutingKey,Headers,Payload},
      consumer_loop(ConsumerState);
    {#'basic.deliver'{consumer_tag=OtherTag},_Msg} ->
      io:format("unexpected deliver to ~p," " error tag:~p~n",[ConsumerTag,OtherTag]),
      consumer_loop(ConsumerState);
    {'DOWN',ChannelRef,_Type,_Object,_Info} ->
      {stop,channel_shutdown};
    {'DOWN',onsumerRef,_Type,_Object,_Info} ->
      io:format("Consumer Shutdown, begin to cancel~n"),
      basic_cancel(Channel,ConsumerTag),
      {stop,consumer_shutdown};
    stop ->
      basic_cancel(Channel,ConsumerTag),
      {stop,normal};
    exit ->
      {stop,normal};
    Msg ->
      error_logger:error_msg("amqp consumer received " "unexpected msg:~n~p~n",[Msg]),
      consumer_loop(ConsumerState)
  end.

-spec bind(pid(),string(),string(),string()) -> ok.
bind(Channel,Exchange,Queue,RK) ->
  QueueBind=#'queue.bind'{arguments=[]
                         ,exchange=binary(Exchange)
                         ,nowait=false
                         ,queue=binary(Queue)
                         ,routing_key=binary(RK)
                         },
  #'queue.bind_ok'{}=amqp_channel:call(Channel,QueueBind),
  ok.

-spec unbind(pid(),string(),string(),string()) -> ok.
unbind(Channel,Exchange,Queue,RK) ->
  QueueUnbind=#'queue.unbind'{arguments=[]
                             ,exchange=binary(Exchange)
                             ,queue=binary(Queue)
                             ,routing_key=RK
                             },
  #'queue.unbind_ok'{}=amqp_channel:call(Channel,QueueUnbind),
  ok.

-spec ack_message(pid(),non_neg_integer()) -> ok.
ack_message(Channel,DeliveryTag) ->
  Method=#'basic.ack'{delivery_tag=DeliveryTag,multiple=false},
  amqp_channel:call(Channel,Method).

basic_cancel(Channel,ConsumerTag) ->
  BasicCancel=#'basic.cancel'{consumer_tag=ConsumerTag,nowait=false},
  #'basic.cancel_ok'{consumer_tag=ConsumerTag}=amqp_channel:call(Channel,BasicCancel),
  ok.

-spec publish(string()) -> no_return() |
                           {noreply,map(),non_neg_integer()} |
                           {stop,atom(),map()}.
publish(Msg) when is_map(Msg) ->
  cast({publish,jsx:encode(Msg)}).

-spec retrieve(pid(),string()) -> ok.
retrieve(Channel,Queue) ->
  GetMethod=#'basic.get'{queue=Queue,no_ack=false},
  case amqp_channel:call(Channel,GetMethod) of
    {#'basic.get_ok'{delivery_tag=DeliveryTag,redelivered=_Redelivered,exchange=_Exchange,routing_key=_RK,message_count=_MessageCount},Content} ->
      log('basic.get_ok',"start"),
      io:format("Received message [~p]:~p~n", [DeliveryTag,Content#amqp_msg.payload]),
      amqp_channel:cast(Channel,#'basic.ack'{delivery_tag=DeliveryTag}),
      ok;
    {'basic.get_empty',?RABBIT_EMPTY_STRING} ->
      log('basic.get_empty',"no message"),
      io:format("Queue is empty, no message received.~n"),
      ok;
    {error,Reason} ->
      log('basic.get',"error"),
      io:format("Error during basic.get:~p~n",[Reason]),
      ok
  end.

-spec start_link(
        Config::list(),
        Exchange::string(),
        Queue::string(),
        Type::string(),
        RK::string(),
        ConsumerTag::string()) -> {ok,pid()} |
                                  atom() |
                                  {error,{already_started,pid()}}.
start_link(Config,Exchange,Queue,Type,RK,ConsumerTag) when is_list(Config);
                                                           is_binary(Exchange);
                                                           is_binary(Queue);
                                                           is_binary(Type);
                                                           is_binary(RK);
                                                           is_binary(ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[Config,Exchange,Queue,Type,RK,ConsumerTag],[]).

-spec init(list()) -> {ok,map()} |
                      {ok,map(),non_neg_integer} |
                      ignore |
                      {stop,atom()}.
init([Config,Exchange,Queue,Type,RK,ConsumerTag]) ->
  process_flag(trap_exit,true),
  {ok,Connection}=amqp_connection:start(amqp_params(amqp_args(Config))),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{}=amqp_channel:call(Channel
                                            ,#'exchange.declare'{arguments=[{"main-exchange",longstr,Exchange}]
                                                                ,auto_delete=false
                                                                ,durable=true
                                                                ,exchange=binary(Exchange)
                                                                ,internal=false
                                                                ,nowait=false
                                                                ,passive=false
                                                                ,type=binary(Type)
                                                                }),

  Uniq=base64:encode(erlang:md5(term_to_binary(make_ref()))),
  FQueue= <<"client.fanout.",Uniq/binary>>,
  ?DBG("FQueue:~p",[FQueue]),
  FQueueDeclare=#'queue.declare'{arguments=[{"x-ha-policy",longstr,"nodes"}
                                           ,{"x-ha-nodes",array,[{longstr,"lugatex@yahoo.com"}]}
                                           ],queue=binary(FQueue),exclusive=true,auto_delete=true,durable=true,nowait=false,passive=false},
  QueueDeclare=#'queue.declare'{arguments=[{"x-ha-policy",longstr,"nodes"}
                                          ,{"x-ha-nodes",array,[{longstr,"lugatex@yahoo.com"}]}
                                          ],queue=binary(Queue),exclusive=true,auto_delete=true,durable=false,nowait=false,passive=false},
  #'queue.declare_ok'{queue=FQueue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,FQueueDeclare),
  #'queue.declare_ok'{queue=Queue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{arguments=[],queue=FQueue,exchange=binary(Exchange),routing_key=binary(RK),nowait=false}),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{arguments=[],queue=Queue,exchange=binary(Exchange),routing_key=binary(RK),nowait=false}),
  ?DBG("Uniq: ~p", [Uniq]),
  erlang:monitor(process, Channel),
  io:format(" [*] Waiting for messages. To_ exit press CTRL+C~n"),
  {ok,#maui_server{
         channel=Channel,
         connection=Connection,
         consumer_count=ConsumerCount,
         consumer_tag=ConsumerTag,
         exchange=binary(Exchange),
         message_count=MessageCount,
         queue=Queue,
         routing_key=binary(RK),
         uniq=FQueue
       },timeout_millseconds()}.

-spec handle_call(term(),atom(),map()) -> {reply,atom(),map()} |
                                          {reply,atom(),map(),non_neg_integer()} |
                                          no_return() |
                                          {noreply,map(),non_neg_integer()} |
                                          {stop,atom(),atom(),map()} |
                                          {stop,atom(),map()}.
handle_call({exchange,Name,Type},_From,#maui_server{channel=Channel}=State) ->
  Declare=#'exchange.declare'{arguments = [{"main-exchange",longstr,Name}]
                             ,auto_delete=false
                             ,durable=true
                             ,exchange=binary(Name)
                             ,internal=false
                             ,nowait=false
                             ,passive=false
                             ,type=binary(Type)
                             },
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,Declare),
  {reply,ok,State,timeout_millseconds()};
handle_call({delete_exchange,Name},_From,#maui_server{channel=Channel}=State) ->
  ExchangeDelete=#'exchange.delete'{exchange=binary(Name),nowait=false},
  #'exchange.delete_ok'{}=amqp_channel:call(Channel,ExchangeDelete),
  {reply,ok,State,timeout_millseconds()};
handle_call({queue,Name},_From,#maui_server{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  QueueDeclare=#'queue.declare'{arguments=[],auto_delete=false,durable=true,exclusive=false,nowait=false,passive=false,queue=binary(Name)},
  #'queue.declare_ok'{queue=Queue,message_count=_MessageCount,consumer_count=_ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{arguments=[],exchange=Exchange,nowait=false,queue=Queue,routing_key=RK}),
  {reply,ok,State,timeout_millseconds()};
handle_call({delete_queue,Name},_From,#maui_server{channel=Channel}) ->
  QueueDelete=#'queue.delete'{queue=binary(Name),nowait=false},
  #'queue.delete_ok'{message_count=MessageCount}=amqp_channel:call(Channel,QueueDelete),
  {reply,ok,#maui_server{channel=Channel,message_count=MessageCount-1},timeout_millseconds()};
handle_call({bind,Exchange,Queue,RK},_From,#maui_server{channel=Channel}=State) ->
  QueueBind=#'queue.bind'{arguments=[]
                         ,exchange=binary(Exchange)
                         ,nowait=false
                         ,queue=binary(Queue)
                         ,routing_key=binary(RK)
                         },
  #'queue.bind_ok'{}=amqp_channel:call(Channel,QueueBind),
  {reply,ok,State,timeout_millseconds()};
handle_call({unbind,Exchange,Queue,RK},_From,#maui_server{channel=Channel}=State) ->
  QueueUnbind=#'queue.unbind'{arguments=[]
                             ,exchange=binary(Exchange)
                             ,queue=binary(Queue)
                             ,routing_key=RK
                             },
  #'queue.unbind_ok'{}=amqp_channel:call(Channel,QueueUnbind),
  {reply,ok,State,timeout_millseconds()};
handle_call({ack_message,DeliveryTag},_From,#maui_server{channel=Channel}=State) ->
  Method=#'basic.ack'{delivery_tag=DeliveryTag,multiple=false},
  amqp_channel:call(Channel,Method),
  {reply,ok,State,timeout_millseconds()};
handle_call(basic_cancel,_From,#maui_server{channel=Channel,consumer_tag=ConsumerTag}=State) ->
  BasicCancel=#'basic.cancel'{consumer_tag=ConsumerTag,nowait=false},
  #'basic.cancel_ok'{consumer_tag=ConsumerTag}=amqp_channel:call(Channel,BasicCancel),
  {reply,ok,State,timeout_millseconds()};
handle_call(retrieve,_From,#maui_server{channel=Channel,queue=Queue}=State) ->
  Method=#'basic.get'{queue=Queue,no_ack=false},
  case amqp_channel:call(Channel,Method) of
    {#'basic.get_ok'{delivery_tag=DeliveryTag,redelivered=_Redelivered,exchange=_Exchange,routing_key=_RK,message_count=_MessageCount},Content} ->
      Msg=jsx:decode(Content#amqp_msg.payload),
      io:format("Received message [~p]:~p~n", [DeliveryTag,Msg]),
      amqp_channel:cast(Channel,#'basic.ack'{delivery_tag=DeliveryTag}),
      {reply,ok,State,timeout_millseconds()};
    {'basic.get_empty',<<>>} ->
      io:format("Queue is empty, no message received.~n"),
      {reply,ok,State,timeout_millseconds()};
    {error,Reason} ->
      io:format("Error during basic.get:~p~n",[Reason]),
      {reply,ok,State,timeout_millseconds()}
  end;
handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

-spec handle_cast(tuple(),map()) -> no_return() |
                                    {noreply,map(),non_neg_integer()} |
                                    {stop,atom(),map()}.
handle_cast(consumer,#maui_server{channel=Channel,queue=Queue,message_count=_MessageCount,consumer_count=_ConsumerCount}=State) ->
  consumer_start(Channel,Queue,self()),
  {noreply,State,timeout_millseconds()};
handle_cast({publish,Msg},#maui_server{channel=Channel,exchange=Exchange,queue=Queue,routing_key=RK,message_count=_MessageCount,consumer_count=_ConsumerCount}=State) ->
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitType}=application:get_env(server,rabbit_type),
  Headers=[{<<"company">>,binary,<<"StarTech">>}],
  BasicPublish=#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},
  Props=#'P_basic'{app_id=?RABBIT_APP_ID
                  ,cluster_id=?RABBIT_APP_ID
                  ,content_encoding=?RABBIT_CONTENT_ENCODING
                  ,content_type=?RABBIT_CONTENT_TYPE
                  ,correlation_id=?RABBIT_CORRELATION_ID
                  ,delivery_mode=?RABBIT_PERSISTENT_DELIVERY
                  ,expiration=?RABBIT_EXPIRATION
                  ,headers=Headers
                  ,message_id=generate_msg_id()
                  ,priority=?RABBIT_PRIORITY
                  ,reply_to=Queue
                  ,timestamp=time_since_epoch()
                  ,type=RabbitType
                  ,user_id=RabbitUsername
                  },
  ok=amqp_channel:cast(Channel,BasicPublish,#'amqp_msg'{props=Props,payload=binary(Msg)}),
  Message = "Published\nPayload: ~p~n",
  io:format(Message,[jsx:decode(Msg)]),
  {noreply,State,timeout_millseconds()};
handle_cast(_Msg,State) -> {noreply,State}.

-spec handle_info(atom(),map()) -> no_return() |
                                   {noreply,map(),non_neg_integer()} |
                                   {stop,atom(),map()}.
handle_info(timeout,#maui_server{channel=Channel,exchange=Exchange,queue=Queue,routing_key=RK,message_count=_MessageCount,consumer_count=_ConsumerCount}=State) ->
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitType}=application:get_env(server,rabbit_type),
  Headers=[{<<"company">>,binary,<<"StarTech">>}],
  amqp_channel:register_return_handler(Channel,self()),
  amqp_channel:register_confirm_handler(Channel,self()),
  Term=#{age=>generate_age(),city=>generate_city(),name=>generate_name()},
  Props=#'P_basic'{app_id=?RABBIT_APP_ID
                  ,cluster_id=?RABBIT_APP_ID
                  ,content_encoding=?RABBIT_CONTENT_ENCODING
                  ,content_type=?RABBIT_CONTENT_TYPE
                  ,correlation_id=?RABBIT_CORRELATION_ID
                  ,delivery_mode=?RABBIT_PERSISTENT_DELIVERY
                  ,expiration=?RABBIT_EXPIRATION
                  ,headers=Headers
                  ,message_id=generate_msg_id()
                  ,priority=?RABBIT_PRIORITY
                  ,reply_to=Queue
                  ,timestamp=time_since_epoch()
                  ,type=RabbitType
                  ,user_id=RabbitUsername
                  },
  Msg=#'amqp_msg'{props=Props,payload=jsx:encode(Term)},
  BasicPublish=#'basic.publish'{exchange=Exchange,immediate=false,mandatory=true,routing_key=RK},
  ok=amqp_channel:cast(Channel,BasicPublish,Msg),
  timer:sleep(?DELAY),
  io:format("~s~n",[jsx:encode(Term)]),
  {noreply,State,timeout_millseconds()};
handle_info({deliver,RK,Header,Payload},#maui_server{routing_key=RK}=State) ->
  io:format("demo  header: ~p~n",[Header]),
  io:format("demo payload: ~p~n", [jsx:decode(Payload)]),
  {noreply,State,timeout_millseconds()};
handle_info({#'basic.return'{reply_code=_ReplyCode,reply_text=_ReplyText,exchange=Exchange,routing_key=_RK},_Payload},State) ->
  error_logger:error_msg("unroutable (~p): ~p",[?SERVER,Exchange]),
  {noreply,State};
handle_info(shutdown,State) -> {stop,normal,State};
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

-if(?OTP_RELEASE > 23).
-else.
-endif.
%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec config() -> #amqp_params_network{
                          connection_timeout :: non_neg_integer(),
                          host :: string(),
                          password :: string(),
                          port :: non_neg_integer(),
                          ssl_options :: atom(),
                          username :: string()
                         }.
config() ->
  #amqp_params_network{
     connection_timeout=?RABBIT_CONNECTION_TIMEOUT,
     heartbeat=?RABBIT_HEARFBEAT,
     host=?RABBIT_HOST,
     password=?RABBIT_PASSWORD,
     port=?RABBIT_PORT,
     ssl_options=?RABBIT_SSL_OPTIONS,
     username=?RABBIT_USERNAME,
     virtual_host=?RABBIT_VIRTUAL_HOST
  }.

-spec basic_test() -> ok.
basic_test() ->
  Durable                      = true,
  Msg1                         = jsx:encode(?RABBIT_TEST_PAYLOAD1),
  Msg2                         = jsx:encode(?RABBIT_TEST_PAYLOAD2),
  Msg3                         = jsx:encode(?RABBIT_TEST_PAYLOAD3),
  Queue1                       = ?RABBIT_TEST_QUEUE1,
  Queue2                       = ?RABBIT_TEST_QUEUE2,
  Queue3                       = ?RABBIT_TEST_QUEUE3,
  RK1                          = ?RABBIT_TEST_ROUTING_KEY1,
  RK2                          = ?RABBIT_TEST_ROUTING_KEY2,
  RK3                          = ?RABBIT_TEST_ROUTING_KEY3,
  Type                         = ?RABBIT_TEST_TYPE,
  Exchange1                    = ?RABBIT_TEST_EXCHANGE1,
  Exchange2                    = ?RABBIT_TEST_EXCHANGE2,
  Exchange3                    = ?RABBIT_TEST_EXCHANGE3,
  Exchanges1                   = [{Exchange1,Type,Durable}],
  Exchanges2                   = [{Exchange2,Type,Durable}],
  Exchanges3                   = [{Exchange3,Type,Durable}],
  ok                           = declare_exchanges(Exchanges1,Queue1,RK1),
  ok                           = declare_exchanges(Exchanges1,Queue2,RK1),
  ok                           = declare_exchanges(Exchanges1,Queue3,RK1),
  ok                           = declare_exchanges(Exchanges1,Queue1,RK2),
  ok                           = declare_exchanges(Exchanges1,Queue2,RK2),
  ok                           = declare_exchanges(Exchanges1,Queue3,RK2),
  ok                           = declare_exchanges(Exchanges1,Queue1,RK3),
  ok                           = declare_exchanges(Exchanges1,Queue2,RK3),
  ok                           = declare_exchanges(Exchanges1,Queue3,RK3),
  ok                           = declare_exchanges(Exchanges2,Queue1,RK1),
  ok                           = declare_exchanges(Exchanges2,Queue2,RK1),
  ok                           = declare_exchanges(Exchanges2,Queue3,RK1),
  ok                           = declare_exchanges(Exchanges2,Queue1,RK2),
  ok                           = declare_exchanges(Exchanges2,Queue2,RK2),
  ok                           = declare_exchanges(Exchanges2,Queue3,RK2),
  ok                           = declare_exchanges(Exchanges2,Queue1,RK3),
  ok                           = declare_exchanges(Exchanges2,Queue2,RK3),
  ok                           = declare_exchanges(Exchanges2,Queue3,RK3),
  ok                           = declare_exchanges(Exchanges3,Queue1,RK1),
  ok                           = declare_exchanges(Exchanges3,Queue2,RK1),
  ok                           = declare_exchanges(Exchanges3,Queue3,RK1),
  ok                           = declare_exchanges(Exchanges3,Queue1,RK2),
  ok                           = declare_exchanges(Exchanges3,Queue2,RK2),
  ok                           = declare_exchanges(Exchanges3,Queue3,RK2),
  ok                           = declare_exchanges(Exchanges3,Queue1,RK3),
  ok                           = declare_exchanges(Exchanges3,Queue2,RK3),
  ok                           = declare_exchanges(Exchanges3,Queue3,RK3),
  ok                           = declare_publish(Exchanges1,Msg1,RK1),
  ok                           = declare_publish(Exchanges1,Msg2,RK1),
  ok                           = declare_publish(Exchanges1,Msg3,RK1),
  ok                           = declare_publish(Exchanges1,Msg1,RK2),
  ok                           = declare_publish(Exchanges1,Msg2,RK2),
  ok                           = declare_publish(Exchanges1,Msg3,RK2),
  ok                           = declare_publish(Exchanges1,Msg1,RK3),
  ok                           = declare_publish(Exchanges1,Msg2,RK3),
  ok                           = declare_publish(Exchanges1,Msg3,RK3),
  ok                           = declare_publish(Exchanges2,Msg1,RK1),
  ok                           = declare_publish(Exchanges2,Msg2,RK1),
  ok                           = declare_publish(Exchanges2,Msg3,RK1),
  ok                           = declare_publish(Exchanges2,Msg1,RK2),
  ok                           = declare_publish(Exchanges2,Msg2,RK2),
  ok                           = declare_publish(Exchanges2,Msg3,RK2),
  ok                           = declare_publish(Exchanges2,Msg1,RK3),
  ok                           = declare_publish(Exchanges2,Msg2,RK3),
  ok                           = declare_publish(Exchanges2,Msg3,RK3),
  ok                           = declare_publish(Exchanges3,Msg1,RK1),
  ok                           = declare_publish(Exchanges3,Msg2,RK1),
  ok                           = declare_publish(Exchanges3,Msg3,RK1),
  ok                           = declare_publish(Exchanges3,Msg1,RK2),
  ok                           = declare_publish(Exchanges3,Msg2,RK2),
  ok                           = declare_publish(Exchanges3,Msg3,RK2),
  ok                           = declare_publish(Exchanges3,Msg1,RK3),
  ok                           = declare_publish(Exchanges3,Msg2,RK3),
  ok                           = declare_publish(Exchanges3,Msg3,RK3),
  ok                           = wait_for_connections(?DEFAULT_TIMEOUT),
  [<<131,97,1>>,<<131,97,2>>,<<131,97,3>>,<<131,97,4>>]=basic_dataset(),
  ok.

-spec declare_exchanges(string(),string(),string()) -> ok.
declare_exchanges(Exchanges,Queue,RK)
  when is_list(Exchanges),
       is_binary(Queue),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(config()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  amqp_channel:register_return_handler(Channel,self()),
  amqp_channel:register_confirm_handler(Channel,self()),
  amqp_channel:call(Channel,#'confirm.select'{nowait=true}),
  [#'exchange.declare_ok'{}=amqp_channel:call(Channel
                                             ,#'exchange.declare'{arguments=[{"main-exchange",longstr,Name}]
                                                                 ,exchange=Name
                                                                 ,type=Type
                                                                 ,durable=Durable}) || {Name,Type,Durable} <- Exchanges],
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
  QueueDeclare=#'queue.declare'{arguments=[{"x-ha-policy",longstr,"nodes"}
                                          ,{"x-ha-nodes",array,[{longstr,"lugatex@yahoo.com"}]}
                                          ],queue=Queue,exclusive=false,auto_delete=false,durable=true},
  #'queue.declare_ok'{queue=Queue}=amqp_channel:call(Channel,QueueDeclare),
  [#'queue.bind_ok'{}=amqp_channel:call(Channel
                                       ,#'queue.bind'{queue=Queue,exchange=element(1,E),routing_key=RK}) || E <- Exchanges],
  ok.

-spec declare_publish(string(),binary(),string()) -> ok.
declare_publish(Exchanges,Msg,RK)
  when is_list(Exchanges),
       is_binary(Msg),
       is_binary(RK) ->
  {ok,Connection}=amqp_connection:start(config()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  [Publish]=[#'basic.publish'{exchange=element(1,E),mandatory=true,routing_key=RK} || E <- Exchanges],
  amqp_channel:call(Channel,Publish,#amqp_msg{payload=Msg}),
  ok=ack_message(Channel,?RABBIT_PERSISTENT_DELIVERY),
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
