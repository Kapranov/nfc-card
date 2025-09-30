%%%-------------------------------------------------------------------
%% @doc core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(core_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(DBConfig) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DBConfig]).

init([DBConfig]) ->
    PoolConfig = DBConfig#{
                           queue => true,
                           trace => false,
                           decode_opts => []},
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = #{id => sdp_pool,
                   start => {pgo_pool, start_link, [default, PoolConfig]}},
    {ok, {SupFlags, [ChildSpecs]}}.
