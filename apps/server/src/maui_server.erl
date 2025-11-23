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
     connection_timeout=?TIMEOUT,
     host=?HOST,
     password=?PASSWORD,
     port=?PORT,
     ssl_options=?SSL,
     username=?USERNAME
  }.

-spec start_link(
        Exchange::string(),
        Queue::string(),
        Type::boolean(),
        RK::string(),
        ConsumerTag::string()) -> {ok,pid()} | atom() | {error,{already_started,pid()}}.
start_link(Exchange,Queue,Type,RK,ConsumerTag) when is_binary(Exchange);
                                                    is_binary(Queue);
                                                    is_boolean(Type);
                                                    is_binary(RK);
                                                    is_binary(ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[Exchange,Queue,Type,RK,ConsumerTag],[]).

-spec stop() -> {reply,atom(),map()} | {reply,atom(),map(),non_neg_integer()} | no_return() | {noreply,map(),non_neg_integer()} | {stop,atom(),atom(),map()} | {stop,atom(),map()}.
stop() -> gen_server:call(?SERVER,stop,infinity).

-spec publish(string()) -> no_return() | {noreply,map(),non_neg_integer()} | {stop,atom(),map()}.
publish(Msg) when is_map(Msg) ->
  gen_server:cast(?SERVER,{publish,jsx:encode(Msg)}).

-spec init(list()) -> {ok,map()} | {ok,map(),non_neg_integer} | ignore | {stop,atom()}.
init([Exchange,Queue,Type,RK,ConsumerTag]) ->
  process_flag(trap_exit,true),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{exchange=binary(Exchange),type=binary(Type),durable=true}),
  QueueDeclare=#'queue.declare'{queue=binary(Queue),exclusive=false,auto_delete=false,durable=true},
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

-spec handle_call(atom(),atom(),map()) -> {reply,atom(),map()} | {reply,atom(),map(),non_neg_integer()} | no_return() | {noreply,map(),non_neg_integer()} | {stop,atom(),atom(),map()} | {stop,atom(),map()}.
handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

-spec handle_cast(tuple(),map()) -> no_return() | {noreply,map(),non_neg_integer()} | {stop,atom(),map()}.
handle_cast({publish,Msg},#maui_server{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  Headers=[{<<"company">>,binary,<<"StarTech">>}],
  BasicPublish=#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},
  Props=#'P_basic'{delivery_mode=?PERSISTENT_DELIVERY,
                   content_type=?CONTENT_TYPE,
                   message_id=generate_msg_id(),
                   timestamp=time_since_epoch(),
                   headers=Headers
                  },
  ok=amqp_channel:cast(Channel,BasicPublish,#'amqp_msg'{props=Props,payload=binary(Msg)}),
  Message = "Published\nPayload: ~p~n",
  io:format(Message, [jsx:decode(Msg)]),
  {noreply,State};
handle_cast(_Msg,State) -> {noreply,State}.

-spec handle_info(atom(),map()) -> no_return() | {noreply,map(),non_neg_integer()} | {stop,atom(),map()}.
handle_info(timeout,#maui_server{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  Term=#{age=>generate_age(),city=>generate_city(),name=>generate_name()},
  Props=#'P_basic'{delivery_mode=?PERSISTENT_DELIVERY,content_type=?CONTENT_TYPE},
  Msg=#'amqp_msg'{props=Props,payload=jsx:encode(Term)},
  amqp_channel:cast(Channel,#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},Msg),
  io:format("~s~n", [jsx:encode(Term)]),
  {noreply,State,timeout_millseconds()};
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
