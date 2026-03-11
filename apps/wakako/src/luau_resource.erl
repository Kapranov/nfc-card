-module(luau_resource).
-export([init/1,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

-spec to_html(wrq:reqdata(),term()) -> {iodata(),wrq:reqdata(),term()}.
to_html(ReqData,State) ->
  {"<html><body>Kokua Line: Will bulky pickup take extra-large sectional sofa?</body></html>",ReqData,State}.
