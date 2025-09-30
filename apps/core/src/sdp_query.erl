-module(sdp_query).

-export([get/1,
         get/2,
         run/2]).

get(Name) ->
    persistent_term:get({?MODULE, Name}).

get(Name, Params) ->
    lists:map(fun(Key) when is_atom(Key) ->
                      proplists:get_value(Key, Params);
                 (S) ->
                      S
              end, ?MODULE:get(Name)).

run(QueryName, Args) ->
    pgo:query(?MODULE:get(QueryName), Args).
