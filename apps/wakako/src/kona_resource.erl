-module(kona_resource).
-export([init/1,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

-spec to_html(wrq:reqdata(),term()) -> {iodata(),wrq:reqdata(),term()}.
to_html(ReqData,State) ->
  {"<html><body>Kokua Line: Robbery case opened after woman, 20, shot in Pearl City</body></html>",ReqData,State}.
