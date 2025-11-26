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

start_link() ->
  supervisor:start_link({local,?SERVER},?SERVER,[]).

init([]) ->
  {ok,RabbitType}=application:get_env(server,rabbit_type),
  {ok,RabbitConsumer}=application:get_env(server,rabbit_consumer1),
  {ok,RabbitExchange}=application:get_env(server,rabbit_exchange1),
  {ok,RabbitQueue}=application:get_env(server,rabbit_queue1),
  {ok,RabbitRoutingKey}=application:get_env(server,rabbit_routing_key1),
  ChildSpecs = ?CHILD(maui_server,
                      maui_server,
                      worker,
                      [RabbitExchange,
                       RabbitQueue,
                       RabbitType,
                       RabbitRoutingKey,
                       RabbitConsumer
                      ]),
  {ok,{{one_for_all,0,1},[ChildSpecs]}}.
