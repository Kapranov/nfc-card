-module(nfc_card).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([start/0, stop/0]).

start() ->
  ok = application:start(crypto),
  application:start(nfc_card).

stop() ->
  application:stop(nfc_card).
