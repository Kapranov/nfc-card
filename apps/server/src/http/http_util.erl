-module(http_util).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-export([amqp_config/0,start_rabbitmq_channel/3]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-spec binary(atom() | list() | binary()) -> binary().
binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

-spec amqp_config() -> [tuple()].
amqp_config() ->
  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
  {ok,RabbitHost}=application:get_env(server,rabbit_host),
  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
  {ok,RabbitPort}=application:get_env(server,rabbit_port),
  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
  [{connection_timeout,RabbitConnectionTimeout}
  ,{heartbeat,RabbitHeartbeat}
  ,{host,RabbitHost}
  ,{password,RabbitPassword}
  ,{port,RabbitPort}
  ,{ssl_options,RabbitSSLOptions}
  ,{username,RabbitUsername}
  ,{virtual_host,RabbitVirtualHost}
  ].

-spec amqp_params(map()) -> map().
amqp_params(Config) ->
   amqp_options:parse(Config).

-spec start_rabbitmq_channel(list(),binary(),binary()) -> {ok,pid()}.
start_rabbitmq_channel(Config,Name,Type) ->
  {ok,Connection}=amqp_connection:start(amqp_params(Config)),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  ExchangeDeclare=#'exchange.declare'{arguments=[]
                                     ,auto_delete=false
                                     ,durable=true
                                     ,exchange=binary(Name)
                                     ,internal=false
                                     ,nowait=false
                                     ,passive=false
                                     ,type=binary(Type)},
  #'exchange.declare_ok'{}=amqp_channel:call(Channel,ExchangeDeclare),
  {ok,Channel}.
