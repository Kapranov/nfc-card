%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CONSUMER,<<"test_bisque1">>).
-define(EXCHANGE,<<"test_lahaina1">>).
-define(QUEUE,<<"test_aloha1">>).
-define(ROUTING,<<"test_mahalo1">>).
-define(SERVER,?MODULE).
-define(TYPE,<<"fanout">>).

start_link() ->
    supervisor:start_link({local,?SERVER},?SERVER,[]).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
                  {maui_server,
                   {maui_server,start_link,
                    [?EXCHANGE,?QUEUE,?TYPE,?ROUTING,?CONSUMER]
                   },permanent,10000,worker,[maui_server]
                  }
                 ],
    {ok, {SupFlags,ChildSpecs}}.
