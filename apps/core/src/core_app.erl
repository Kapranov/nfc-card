-module(core_app).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").
-behaviour(application).
-export([ensure_started/1,start/0,start/2,stop/0,stop/1]).

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

-spec start() -> ok.
start() ->
  application:start(core).

-spec start(any(),any()) -> tuple().
start(_StartType,_StartArgs) ->
  {ok,DBConfig}=application:get_env(core,db_config),
  core_sup:start_link(DBConfig).

-spec stop() -> ok.
stop() ->
  Res=application:stop(core),
  Res.

-spec stop(any()) -> ok.
stop(_State) -> ok.
