-module(wahiawa).

-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,publish/1,start_link/5,stop/0,terminate/2]).
-export([start_consumer/1,receive_messages/1,get_msg/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-record(state,{channel,connection,consumer_count,consumer_tag,exchange,queue,message_count,routing_key}).

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

log(Key,Value) -> io:format("~p: ~p~n",[Key,Value]).

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

publish(Message) -> gen_server:cast(?SERVER,{publish,Message}).

get_msg() -> gen_server:call(?SERVER, get_msg).

stop() -> gen_server:call(?SERVER,stop,infinity).

init([Exchange,Queue,Type,RoutingKey,ConsumerTag]) ->
  {ok,Connection} = amqp_connection:start(amqp_params()),
  {ok,Channel} = amqp_connection:open_channel(Connection),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel,#'exchange.declare'{exchange=binary(Exchange),type=binary(Type)}),
  QueueDeclare=#'queue.declare'{queue=binary(Queue),exclusive=false},
  #'queue.declare_ok'{queue=Queue,message_count=MessageCount,consumer_count=ConsumerCount}=amqp_channel:call(Channel,QueueDeclare),
  #'queue.bind_ok'{}=amqp_channel:call(Channel,#'queue.bind'{queue=Queue,exchange=binary(Exchange),routing_key=binary(RoutingKey)}),
  {ok, #state{
          channel=Channel,
          connection=Connection,
          consumer_count=ConsumerCount,
          consumer_tag=ConsumerTag,
          exchange=binary(Exchange),
          message_count=MessageCount,
          queue=Queue,
          routing_key=binary(RoutingKey)
         },timeout_millseconds()}.

handle_call(get_msg,_From,State) ->
  BasicConsume=#'basic.consume'{queue=State#state.queue,consumer_tag=State#state.consumer_tag,no_ack=true},
  #'basic.consume_ok'{consumer_tag=ConsumerTag}=amqp_channel:subscribe(State#state.channel,BasicConsume,self()),
  receive
    #'basic.consume_ok'{consumer_tag=ConsumerTag} -> ok
  end,
  log(read_messages,"start"),
  Msg=ttt(State#state.channel),
  io:format("Msg: ~p~n",[Msg]),
  log(read_messages,"finish"),
  log(basic_cancel,"start"),
  %BasicCancel=#'basic.cancel'{consumer_tag=State#state.consumer_tag},
  %#'basic.cancel_ok'{consumer_tag=ConsumerTag}=amqp_channel:call(State#state.channel,BasicCancel),
  io:format("ConsumerStop: ~p~n",[ConsumerTag]),
  {reply,ok,State};

handle_call(stop,_From,State) ->
    {stop,normal,ok,State};

handle_call(_Request,_From,State) ->
  Reply=ok,
  {reply,Reply,State}.

handle_cast({publish,Message},State) ->
  log(publish,"basic.publish setup"),
  BasicPublish=#'basic.publish'{exchange=State#state.exchange,routing_key=State#state.routing_key},
  ok=amqp_channel:cast(State#state.channel,BasicPublish,#'amqp_msg'{payload=binary(Message)}),
  {noreply,State};

handle_cast(_Msg,State) ->
  {noreply,State}.

handle_info({#'basic.deliver'{},#amqp_msg{}},State) -> {noreply,State};
handle_info({#'basic.consume'{},_},State) -> {noreply,State};
handle_info(#'basic.consume_ok'{},State) -> {noreply,State};
handle_info(#'basic.cancel'{},State) -> {noreply,State};
handle_info(#'basic.cancel_ok'{},State) -> {stop,normal,State};

handle_info(shutdown,State) -> {stop,normal,State};
handle_info({'DOWN', _MRef, process, _Pid, _Info}, State) ->
  {noreply, State};

handle_info(timeout,State) ->
  io:format("Timeout: ~p~n", [State#state.consumer_tag]),
  {noreply,State,timeout_millseconds()}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

start_consumer(State) ->
  log(setup_consumer,"basic.consume"),
  BasicConsume=#'basic.consume'{queue=State#state.queue,consumer_tag = <<"">>,no_ack=true},
  #'basic.consume_ok'{consumer_tag=ConsumerTag}=amqp_channel:subscribe(State#state.channel,BasicConsume,self()),
  log(setup_consumer,"basic.consume_ok start receive"),
  receive
    #'basic.consume_ok'{consumer_tag=ConsumerTag} -> ok
  end,
  log(setup_consumer,"basic.consume_ok finished"),
  log(read_messages,"start"),
  Msg=ttt(State#state.channel),
  io:format("Msg: ~p~n",[Msg]),
  log(read_messages,"finish"),
  log(basic_cancel,"start").
  %BasicCancel=#'basic.cancel'{consumer_tag=ConsumerTag},
  %#'basic.cancel_ok'{consumer_tag=ConsumerTag}=amqp_channel:call(State#state.channel,BasicCancel).

ttt(Channel) ->
  receive
    exit -> ok ;
    {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=_RoutingKey},Content} ->
      log(read_messages,"basic.deliver"),
      #amqp_msg{payload=Payload}=Content,
      io:format("Received message: ~p~n", [binary_to_list(Payload)]),
      amqp_channel:call(Channel,#'basic.ack'{delivery_tag=DeliveryTag}),
      ttt(Channel);
    Any ->
      io:format("received unexpected Any: ~p~n",[Any]),
      ttt(Channel)
    after 4000 ->
      io:format("~n"),
      io:format("Message timeout exceeded ~n")
%   receive
%     exit -> ok ;
%     {#'basic.deliver'{delivery_tag = Tag, exchange = ?EVENTS_EXCHANGE}, #'amqp_msg'{payload = Payload}} ->
%       io:format("Received message: ~p~n", [binary_to_list(Payload)]),
%       amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
%       loop(Channel);
%     _Other ->
%       io:format("Received other message xxxx: ~p~n", [_Other]),
%       loop(Channel)
%   end.
  end.

receive_messages(Channel) ->
  receive
    {#'basic.deliver'{delivery_tag=Tag,exchange=_Exchange,routing_key=_RoutingKey},_Headers,Body} ->
      io:format("Received message: ~p~n",[binary_to_list(Body)]),
      amqp_channel:basic_ack(Channel,Tag,false),
      receive_messages(Channel);
    _OtherMessage ->
      io:format("Received other message: ~p~n",[_OtherMessage]),
      receive_messages(Channel)
  end.

% loop(Channel,ConsumerTag) ->
%   receive
%     #'basic.consume_ok'{} -> loop(Channel,ConsumerTag);
%     #'basic.cancel_ok'{} -> ok;
%     {#'basic.deliver'{delivery_tag = Tag}, Content} ->
%       R = io_lib:format("~p", [Content]),
%       lists:flatten(R),
%       io:fwrite("Received\n"),
%       io:fwrite(R),
%       amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
%       io:format("Worked: ~p~n", [ConsumerTag]),
%       loop(Channel,ConsumerTag)
%   end.

% loop(Channel) ->
%   receive
%     exit -> ok ;
%     {#'basic.deliver'{delivery_tag = Tag, exchange = ?EVENTS_EXCHANGE}, #'amqp_msg'{payload = Payload}} ->
%       io:format("Received message: ~p~n", [binary_to_list(Payload)]),
%       amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
%       loop(Channel);
%     _Other ->
%       io:format("Received other message xxxx: ~p~n", [_Other]),
%       loop(Channel)
%   end.
