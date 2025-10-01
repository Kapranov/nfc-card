-module(rpc_client).

-export([init/0, add/2, find/1, handle/2, generate_uuid/0]).

-import(rpc_server, [rpc/2]).

add(Name, Place) ->
  rpc(rpc_client, {add, Name, Place}).
find(Name)       -> rpc(rpc_client, {find, Name}).

init() -> dict:new().
handle({add, Name, Place}, Dict) ->
  {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict)       -> {dict:find(Name, Dict), Dict}.

generate_uuid() -> uuid:uuid_to_string(uuid:get_v4()).
