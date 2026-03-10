-module(wakako).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([ensure_started/1,start/0,start_link/0,stop/0]).

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

start_link() ->
  %ensure_started(inets),
  %ensure_started(crypto),
  %ensure_started(asn1),
  %ensure_started(public_key),
  %ensure_started(ssl),
  %ensure_started(xmerl),
  %ensure_started(compiler),
  %ensure_started(syntax_tools),
  %ensure_started(mochiweb),
  %application:set_env(webmachine,webmachine_logger_module,webmachine_logger),
  %ensure_started(webmachine),
  wakako_sup:start_link().

-spec start() -> ok.
start() ->
  %ensure_started(inets),
  %ensure_started(crypto),
  %ensure_started(asn1),
  %ensure_started(public_key),
  %ensure_started(ssl),
  %ensure_started(xmerl),
  %ensure_started(compiler),
  %ensure_started(syntax_tools),
  %ensure_started(mochiweb),
  %application:set_env(webmachine,webmachine_logger_module,webmachine_logger),
  %ensure_started(webmachine),
  application:start(wakako).

-spec stop() -> ok.
stop() ->
  Res=application:stop(wakako),
  %application:stop(webmachine),
  %application:stop(mochiweb),
  %application:stop(syntax_tools),
  %application:stop(compiler),
  %application:stop(xmerl),
  %application:stop(ssl),
  %application:stop(public_key),
  %application:stop(asn1),
  %application:stop(crypto),
  %application:stop(inets),
  Res.
