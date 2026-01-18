-module(http_util).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-export([start_rabbitmq_channel/3]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-spec binary(atom() | list() | binary()) -> binary().
binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.

-spec amqp_params(map()) -> map().
amqp_params(Args) ->
   amqp_options:parse(Args).

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

-spec start_rabbitmq_channel(map(),binary(),binary()) -> {ok,pid()}.
start_rabbitmq_channel(Config,Name,Type) ->
  {ok,Connection}=amqp_connection:start(amqp_params(amqp_args(Config))),
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
