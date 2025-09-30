-module(rpc_server).

-behaviour(gen_server).

-export([start_link/1, start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8080).

-record(state, {port, lsock}).

start_link(Port) ->
  gen_server:start_link({local,?SERVER}, ?MODULE, [Port], []).

start_link() ->
  start_link(?DEFAULT_PORT).

stop() ->
  gen_server:cast(?SERVER, stop).

init([Port]) ->
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
  {ok, #state{lsock=LSock, port=Port}, 0}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  do_rpc(Socket, RawData),
  {noreply, State}};

handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_rpc(Socket, RawData) ->
  Data = #{<<"card_uid">> => <<"value1">>, <<"user_id">> => 123},
  JsonString = jsx:encode(Data),
  rpc:call(_Node, _Module, _Function, [JsonString]).
