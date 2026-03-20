-module(kalihi_resource).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-export([init/1]).
-export([allowed_methods/2,content_types_provided/2,is_authorized/2,provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

is_authorized(ReqData,State) ->
  case wrq:disp_path(ReqData) of
    "authdemo" ->
      case wrq:get_req_header("authorization",ReqData) of
        "Basic "++Base64 ->
          Str = base64:mime_decode_to_string(Base64),
          case string:tokens(Str,":") of
            ["waipahu","maui"] ->
              {true,ReqData,State};
            _ ->
              {"Basic realm=webmachine",ReqData,State}
          end;
        _ ->
          {"Basic realm=webmachine",ReqData,State}
      end;
    _ -> {true,ReqData,State}
  end.

allowed_methods(ReqData,State) ->
  Methods=['GET'],
  {Methods,ReqData,State}.

content_types_provided(ReqData,State) ->
  Types=[{"application/json",provide_content}],
  {Types,ReqData,State}.

provide_content(ReqData,State) ->
  Content = #{"report" => <<"Tech View: While AI is fast, capable, users must edit its content">>},
  render_json(Content,ReqData,State).

render_json(Content,ReqData,State) ->
  Body = mochijson2:encode(Content),
  {Body,ReqData,State}.
