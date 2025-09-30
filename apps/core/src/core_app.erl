%%%-------------------------------------------------------------------
%% @doc core public API
%% @end
%%%-------------------------------------------------------------------

-module(core_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, DBConfig} = application:get_env(core, db_config),
    core_sup:start_link(DBConfig).

stop(_State) ->
    ok.
