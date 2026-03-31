-module(wakako_app).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-behaviour(application).
-export([start/2,stop/1]).

-spec start(application:start_type(), term()) -> {error,any()} | {ok,pid()}.
start(_Type,_StartArgs) ->
  wakako_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  application:stop(wakako),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(syntax_tools),
  application:stop(compiler),
  application:stop(xmerl),
  application:stop(ssl),
  application:stop(public_key),
  application:stop(asn1),
  application:stop(crypto),
  application:stop(inets),
  ok.
