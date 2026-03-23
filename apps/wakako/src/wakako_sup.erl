-module(wakako_sup).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-behaviour(supervisor).
-export([init/1
        ,start_link/0
        ,update/0
        ]).

-define(SERVER,?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER},?SERVER,[]).

update() ->
  {ok,{_,Specs}}=init([]),
  Old=sets:from_list([Name || {Name,_,_,_} <- supervisor:which_children(?SERVER)]),
  New=sets:from_list([Name || {Name,_,_,_,_,_} <- Specs]),
  Kill=sets:subtract(Old,New),
  sets:fold(fun (Id, ok) ->
                supervisor:terminate_child(?SERVER,Id),
                supervisor:delete_child(?SERVER,Id)
            end,ok,Kill),
  [supervisor:start_child(?SERVER,Spec) || Spec <- Specs],
  ok.

init([]) ->
  Web={webmachine_mochiweb
      ,{webmachine_mochiweb,start,[wakako_config:web_config()]}
      ,permanent
      ,5000
      ,worker
      ,dynamic
      %,[webmachine_mochiweb]
      },
  Processes=[Web],
  {ok,{{one_for_one,10,10},Processes}}.
