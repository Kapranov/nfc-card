-module(maui_server).
-author('Oleg G.Kapranov').

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

-record(state,{channel,connection,consumer_count,consumer_tag,exchange,queue,message_count,routing_key}).

generate_age() -> fakerl:random(0,99).
generate_city() -> fakerl:random_city().
generate_name() -> fakerl:random_name().

binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

generate_msg_id() ->
  list_to_binary(uuidx:uuid_v4()).

time_since_epoch() ->
  Now=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  Now*1_000.

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

start_link(Exchange,Queue,Type,RK,ConsumerTag) ->
  gen_server:start_link({local,?SERVER},?SERVER,[Exchange,Queue,Type,RK,ConsumerTag],[]).

stop() -> gen_server:call(?SERVER,stop,infinity).

publish(Msg) ->
  gen_server:cast(?SERVER,{publish,jsx:encode(Msg)}).

init([Exchange,Queue,Type,RK,ConsumerTag]) ->
  process_flag(trap_exit,true),
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{exchange=binary(Exchange),type=binary(Type),durable=true}),
  QueueDeclare=#'queue.declare'{queue=binary(Queue),exclusive=false,auto_delete=false,durable=true},
  #'queue.declare_ok'{queue=Queue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=binary(Exchange),routing_key=binary(RK)}),
  io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),
  {ok,#state{
         channel=Channel,
         connection=Connection,
         consumer_count=ConsumerCount+1,
         consumer_tag=ConsumerTag,
         exchange=binary(Exchange),
         message_count=MessageCount+1,
         queue=Queue,
         routing_key=binary(RK)
       },timeout_millseconds()}.

handle_call(stop,_From,State) -> {stop,normal,ok,State};
handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast({publish,Msg},#state{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
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

handle_info(timeout,#state{channel=Channel,exchange=Exchange,routing_key=RK}=State) ->
  Props=#'P_basic'{delivery_mode=?PERSISTENT_DELIVERY,content_type=?CONTENT_TYPE},
  Msg=#'amqp_msg'{props=Props,payload=jsx:encode(?MESSAGE)},
  amqp_channel:cast(Channel,#'basic.publish'{exchange=Exchange,mandatory=true,routing_key=RK},Msg),
  Term=#{age => generate_age(),city => generate_city(), name => generate_name()},
  %Term=#{name => <<"Alice">>,age => 30,city => <<"New York">>},
  io:format("~s~n", [jsx:encode(Term)]),
  {noreply,State,timeout_millseconds()};
handle_info(Info,State) ->
  io:format("unexpected info: ~p~n",[Info]),
  {noreply,State}.

terminate(_Reason,#state{connection=Connection,channel=Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok;
terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.
