%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER,?MODULE).
-define(CONSUMER,<<"bisque">>).
-define(EXCHANGE,<<"lahaina">>).
-define(QUEUE,<<"aloha_queue">>).
-define(ROUTING,<<"aloha_queue">>).
-define(TYPE,<<"fanout">>).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?SERVER, []).

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
    {ok, {SupFlags, ChildSpecs}}.
