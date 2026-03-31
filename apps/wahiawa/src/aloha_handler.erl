-module(aloha_handler).
-export([init/2]).

init(Req,Opts) ->
  Content = iolist_to_binary(jiffy:encode(#{rest => <<"Aloha Erlang!">>})),
  ReqData = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},Content,Req),
  {ok,ReqData,Opts}.
