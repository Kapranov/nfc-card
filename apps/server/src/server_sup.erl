%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER,?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER},?SERVER,[]).

init([]) ->
  {ok,RabbitType}=application:get_env(server,rabbit_type),
  {ok,RabbitConsumer}=application:get_env(server,rabbit_consumer1),
  {ok,RabbitExchange}=application:get_env(server,rabbit_exchange1),
  {ok,RabbitQueue}=application:get_env(server,rabbit_queue1),
  {ok,RabbitRoutingKey}=application:get_env(server,rabbit_routing_key1),
  SupFlags = #{
      strategy => one_for_all,
      intensity => 0,
      period => 1
  },
  ChildSpecs = [
                {maui_server,
                 {maui_server,start_link,
                  [RabbitExchange,RabbitQueue,RabbitType,RabbitRoutingKey,RabbitConsumer]
                 },permanent,10000,worker,[maui_server]
                }
               ],
  {ok, {SupFlags,ChildSpecs}}.
