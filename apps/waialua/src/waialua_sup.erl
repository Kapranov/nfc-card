-module(waialua_sup).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-behaviour(supervisor).
-export([init/1,start_link/0]).

-define(SERVER,?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER},?MODULE,[]).

init([]) ->
  SupFlags = #{
      strategy => one_for_all,
      intensity => 0,
      period => 1
  },
  ChildSpecs=[
    {waialua_client, {waialua_client, start_link, []}, permanent, 10000, worker, [waialua_client]},
    {waialua_consume, {waialua_consume, start_link, []}, permanent, 10000, worker, [waialua_consume]}
  ],
  {ok,{SupFlags,ChildSpecs}}.
