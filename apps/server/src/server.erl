-module(server).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([start/0, stop/0]).

start() ->
  ok = application:start(crypto),
  application:start(server).


stop() ->
  application:stop(server).

