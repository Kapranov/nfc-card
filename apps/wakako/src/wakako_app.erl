-module(wakako_app).
-behaviour(application).
-export([ensure_started/1,start_link/0,start/2,stop/1]).

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

start_link() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(asn1),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(xmerl),
  ensure_started(compiler),
  ensure_started(syntax_tools),
  ensure_started(mochiweb),
  application:set_env(webmachine,webmachine_logger_module,webmachine_logger),
  ensure_started(webmachine),
  wakako_sup:start_link().

-spec start(application:start_type(), term()) -> {error,any()} | {ok,pid()}.
start(_Type,_StartArgs) ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(asn1),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(xmerl),
  ensure_started(compiler),
  ensure_started(syntax_tools),
  ensure_started(mochiweb),
  application:set_env(webmachine,webmachine_logger_module,webmachine_logger),
  ensure_started(webmachine),
  wakako_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
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
