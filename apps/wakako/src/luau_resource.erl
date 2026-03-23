-module(luau_resource).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-export([allowed_methods/2
        ,content_types_provided/2
        ,init/1
        ,provide_content/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

allowed_methods(ReqData,State) ->
  Methods=['GET'],
  {Methods,ReqData,State}.

content_types_provided(ReqData,State) ->
  Types=[{"application/json",provide_content}],
  {Types,ReqData,State}.

provide_content(ReqData,State) ->
  Content = #{"report" => <<"Kokua Line: Will bulky pickup take extra-large sectional sofa?">>},
  render_json(Content,ReqData,State).

render_json(Content,ReqData,State) ->
  Body = mochijson2:encode(Content),
  {Body,ReqData,State}.
