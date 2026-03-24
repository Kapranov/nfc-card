-module(kona_resource).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-export([allowed_methods/2
        ,content_types_provided/2
        ,init/1
        ,is_authorized/2
        ,provide_content/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,[{is_protected, true}]}.

allowed_methods(ReqData,State) ->
  Methods=['GET'],
  {Methods,ReqData,State}.

content_types_provided(ReqData,State) ->
  Types=[{"application/json",provide_content}],
  {Types,ReqData,State}.

provide_content(ReqData,State) ->
  Content = #{"report" => <<"Kokua Line: Robbery case opened after woman, 20, shot in Pearl City">>},
  render_json(Content,ReqData,State).

render_json(Content,ReqData,State) ->
  Body = mochijson2:encode(Content),
  {Body,ReqData,State}.

is_authorized(ReqData,State) ->
  case wrq:get_req_header("Authorization",ReqData) of
    "Basic "++Base64 ->
      Str = base64:mime_decode_to_string(Base64),
      case string:tokens(Str,":") of
        ["waipahu","maui"] ->
          {true,ReqData,State};
        _ ->
          {"Basic realm=webmachine",ReqData,State}
      end;
    _ ->
      case proplists:get_value(is_protected,State,false) of
        false -> {true,ReqData,State};
        _ -> {"Basic realm=webmachine",ReqData,State}
      end
  end.
