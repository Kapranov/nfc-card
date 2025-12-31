-module(amqp_options).

-export([amqp_params/1
        ,declare_callback/2
        ,declare_exchanges/2
        ,declare_publish/4
        ,parse/1
        ,parse/2
        ,send_msg/4
        ,start_consumer/3
        ,stop_consumer/2
        ,stop_consumers/2
        ,word_count_callback/2
        ,word_reverse_callback/2
        ]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-spec amqp_params(list()) -> map().
amqp_params(Config) ->
   #amqp_params_network{connection_timeout=proplists:get_value(connection_timeout,Config,7_000)
                       ,heartbeat=proplists:get_value(heartbeat,Config,80)
                       ,host=proplists:get_value(host,Config,"127.0.0.1")
                       ,password=proplists:get_value(password,Config,<<"guest">>)
                       ,port=proplists:get_value(port,Config,5672)
                       ,ssl_options=proplists:get_value(ssl_options,Config,none)
                       ,username=proplists:get_value(user,Config,<<"guest">>)
                       ,virtual_host=proplists:get_value(virtual_host,Config,<<"/">>)
                       }.

-spec parse(map()) -> map().
parse(Opts) when is_record(Opts, amqp_params_network) ->
  Opts;
parse(Opts) ->
  parse(Opts, #{}).

-spec parse(list() | map(), map()) -> map().
parse([], Acc) -> amqp_params(maps:to_list(Acc));
parse(Opts, Acc) when is_map(Opts) ->
   parse(maps:to_list(Opts), Acc);
parse([{host, Host} | R], Acc) when is_binary(Host) ->
   parse(R, Acc#{host => binary_to_list(Host)});
parse([{host, Host} | R], Acc) when is_list(Host) ->
   parse(R, Acc#{host => Host});
parse([{port, Port} | R], Acc) when is_integer(Port) ->
   parse(R, Acc#{port => Port});
parse([{heartbeat, KeepAlive} | R], Acc) when is_integer(KeepAlive) ->
   parse(R, Acc#{heartbeat => KeepAlive});
parse([{user, User} | R], Acc) when is_binary(User) ->
   parse(R, Acc#{user => User});
parse([{pass, Pass} | R], Acc) when is_binary(Pass) ->
   parse(R, Acc#{pass => Pass});
parse([{vhost, VHost} | R], Acc) when is_binary(VHost) ->
   parse(R, Acc#{vhost => VHost});
parse([{connection_timeout, ConnTimeout} | R], Acc) when is_integer(ConnTimeout) ->
   parse(R, Acc#{connection_timeout => ConnTimeout});
parse([{ssl, false} | R], Acc) ->
   parse(R, Acc#{ssl => false});
parse([_ | R], Acc) ->
   parse(R, Acc).

-spec declare_exchanges(list(),list()) -> atom().
declare_exchanges(Config,Exchanges) ->
  {ok,Connection}=amqp_connection:start(network,Config),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  [#'exchange.declare_ok'{}=amqp_channel:call(Channel,#'exchange.declare'{exchange=Name,type=Type,durable=Durable}) || {Name,Type,Durable} <- Exchanges],
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

declare_callback(_Channel,#amqp_msg{payload=Msg}) ->
  io:format("Got message ~p~n",[Msg]).

declare_publish(Config,Exchange,Msg,RK) ->
  {ok,Connection}=amqp_connection:start(network,Config),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  Publish=#'basic.publish'{exchange=Exchange,routing_key=RK},
  amqp_channel:call(Channel,Publish,#amqp_msg{payload=term_to_binary(Msg)}),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

word_count_callback(_Channel,#amqp_msg{payload=Msg}) ->
  L=length(string:tokens(binary_to_list(Msg)," ")),
  io:format("Count: ~p~n",[L]).

word_reverse_callback(_Channel,#amqp_msg{payload=Msg}) ->
  Words=lists:reverse(string:tokens(binary_to_list(Msg)," ")),
  io:format("Reversed Words: ~p~n",[Words]).

-spec send_msg(list(),binary(),binary(),binary()) -> ok.
send_msg(Config,Exchange,RK,Msg) ->
  {ok,Connection}=amqp_connection:start(network,amqp_params(Config)),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  Publish=#'basic.publish'{exchange=Exchange,routing_key=RK},
  amqp_channel:call(Channel,Publish,#amqp_msg{payload=Msg}),
  ok.

-spec start_consumer(pid(),binary(),binary()) -> {atom(),binary()}.
start_consumer(Channel,Exchange,RK) ->
  #'queue.declare_ok'{queue=Queue}= amqp_channel:call(Channel,#'queue.declare'{exclusive=true,auto_delete=true}),
  QueueBind=#'queue.bind'{queue=Queue,exchange=Exchange,routing_key=RK},
  #'queue.bind_ok'{}=amqp_channel:call(Channel,QueueBind),
  #'basic.consume_ok'{consumer_tag=ConsumerTag}=amqp_channel:subscribe(Channel,#'basic.consume'{queue=Queue,no_ack=true},self()),
  {ok,ConsumerTag}.

-spec stop_consumer(pid(),binary()) -> atom().
stop_consumer(Channel,Consumer) ->
  stop_consumers(Channel,[Consumer]).

-spec stop_consumers(pid(),list()) -> atom().
stop_consumers(_Channel,[]) -> ok;
stop_consumers(Channel,[CTag|T]) ->
  case CTag of
    <<"">> -> ok;
    _ ->
      #'basic.cancel_ok'{}=amqp_channel:call(Channel,#'basic.cancel'{consumer_tag=CTag})
  end,
  stop_consumers(Channel,T).
