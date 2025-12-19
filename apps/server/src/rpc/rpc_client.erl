-module(rpc_client).

-export([init/0, add/2, find/1, handle/2, generate_uuid/0]).

-import(rpc_server, [rpc/2]).

add(Name, Place) ->
  case Name of
    "/card/touch" ->
      rpc(rpc_client, {add, Name, Place});
    "/card/assign" ->
      rpc(rpc_client, {add, Name, Place});
    "/card/delete" ->
      rpc(rpc_client, {add, Name, Place});
    "/card/list_by_user" ->
      rpc(rpc_client, {add, Name, Place});
    "/card/delete_all_by_user" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/set" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/get" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/add_exclusion" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/get_exclusion" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/history_by_user" ->
      rpc(rpc_client, {add, Name, Place});
    "/work_time/statistics_by_user" ->
      rpc(rpc_client, {add, Name, Place});
    _Other ->
      error
  end.

find(Name)       -> rpc(rpc_client, {find, Name}).

init() -> dict:new().
handle({add, Name, Place}, Dict) ->
  {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict)       -> {dict:find(Name, Dict), Dict}.

generate_uuid() -> uuid:uuid_to_string(uuid:get_v4()).
