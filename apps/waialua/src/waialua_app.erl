-module(waialua_app).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-behaviour(application).
-export([start/2,stop/1]).

start(_StartType,_StartArgs) ->
  waialua_sup:start_link().

stop(_State) -> ok.
