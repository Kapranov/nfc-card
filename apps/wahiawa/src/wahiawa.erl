-module(wahiawa).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-export([off/0,start/0,start_link/0,stop/0]).

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

off() ->
  init:stop(),
  halt().

-spec start_link() -> {ok,pid()}.
start_link() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(asn1),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(ranch),
  ensure_started(xmerl),
  ensure_started(cowboy),
  ensure_started(compiler),
  ensure_started(syntax_tools),
  Dispatch = cowboy_router:compile([
    {'_', [{"/", hello_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,[{port, 8888}],#{env => #{dispatch => Dispatch}}),
  wahiawa_sup:start_link().

-spec start() -> 'ok'.
start() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(asn1),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(ranch),
  ensure_started(xmerl),
  ensure_started(cowboy),
  ensure_started(compiler),
  ensure_started(syntax_tools),
  Dispatch = cowboy_router:compile([
    {'_', [{"/", hello_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,[{port, 8888}],#{env => #{dispatch => Dispatch}}),
  %PrivDir = code:priv_dir(wahiawa),
  %{ok, _} = cowboy:start_tls(https,[{port,8888},{certfile, PrivDir ++ "/ssl/cert.pem"},{keyfile, PrivDir ++ "/ssl/key.pem"}], #{env => #{dispatch => Dispatch}}),
  application:start(wahiawa).

-spec stop() -> ok.
stop() ->
  Res=application:stop(wahiawa),
  application:stop(syntax_tools),
  application:stop(compiler),
  application:stop(cowboy),
  application:stop(xmerl),
  application:stop(ranch),
  application:stop(ssl),
  application:stop(public_key),
  application:stop(asn1),
  application:stop(crypto),
  application:stop(inets),
  Res.
