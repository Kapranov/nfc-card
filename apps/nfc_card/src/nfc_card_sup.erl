%%%-------------------------------------------------------------------
%% @doc nfc_card top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nfc_card_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
