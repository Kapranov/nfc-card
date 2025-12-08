%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2,stop/1]).

-spec start(application:start_type(), term()) -> {error,any()} | {ok,pid()}.
start(_StartType, _StartArgs) ->
    server_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
