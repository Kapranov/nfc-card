-module(wakako_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

-define(SERVER,?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER},?SERVER,[]).

init([]) ->
  Web={webmachine_mochiweb
      ,{webmachine_mochiweb,start,[wakako_config:web_config()]}
      ,permanent
      ,5000
      ,worker
      ,[webmachine_mochiweb]
      },
  Processes=[Web],
  {ok,{{one_for_one,10,10},Processes}}.
