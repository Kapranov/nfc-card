%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER,?MODULE).
-define(CHILD(Id,Mod,Type,Args),{Id,{Mod,start_link,Args},transient,5000,Type,[Mod]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local,?SERVER},?SERVER,[]).

init([]) ->
%  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
%  {ok,RabbitConsumer}=application:get_env(server,rabbit_consumer1),
%  {ok,RabbitExchange}=application:get_env(server,rabbit_exchange1),
%  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
%  {ok,RabbitHost}=application:get_env(server,rabbit_host),
%  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
%  {ok,RabbitPort}=application:get_env(server,rabbit_port),
%  {ok,RabbitQueue}=application:get_env(server,rabbit_queue1),
%  {ok,RabbitRoutingKey}=application:get_env(server,rabbit_routing_key1),
%  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
%  {ok,RabbitType}=application:get_env(server,rabbit_type),
%  {ok,RabbitUsername}=application:get_env(server,rabbit_password),
%  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
%  Config=[{connection_timeout,RabbitConnectionTimeout}
%         ,{heartbeat,RabbitHeartbeat}
%         ,{host,RabbitHost}
%         ,{password,RabbitPassword}
%         ,{port,RabbitPort}
%         ,{ssl_options,RabbitSSLOptions}
%         ,{username,RabbitUsername}
%         ,{virtual_host,RabbitVirtualHost}
%         ],
%  ChildSpecs = ?CHILD(maui_server,
%                      maui_server,
%                      worker,
%                      [Config,
%                       RabbitExchange,
%                       RabbitQueue,
%                       RabbitType,
%                       RabbitRoutingKey,
%                       RabbitConsumer
%                      ]),
%  {ok,{{one_for_all,0,1},[ChildSpecs]}}.
  {ok,{{one_for_all,0,1},[]}}.
