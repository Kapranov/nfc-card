-module(hello_handler).
-export([init/2,allowed_methods/2,content_types_provided/2,to_json/2]).

init(Req,Opts) ->
  {cowboy_rest,Req,Opts}.

allowed_methods(Req,State) ->
  {[<<"GET">>],Req,State}.

content_types_provided(Req,State) ->
  {[{{<<"application">>,<<"json">>,[]},to_json}],Req,State}.

to_json(Req, State) ->
  Body = <<"{\"rest\": \"Hello World!\"}">>,
  {Body, Req, State}.
