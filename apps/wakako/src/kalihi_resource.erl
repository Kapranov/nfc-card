-module(kalihi_resource).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-export([allowed_methods/2
        ,content_types_provided/2
        ,expires/2
        ,generate_etag/2
        ,hash_body/1
        ,init/1
        ,is_authorized/2
        ,last_modified/2
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
  Content = #{"report" => <<"Tech View: While AI is fast, capable, users must edit its content">>},
  render_json(Content,ReqData,State).

render_json(Content,ReqData,State) ->
  Body = mochijson2:encode(Content),
  {Body,ReqData,State}.

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

expires(ReqData,State) ->
  {{{2026,1,1},{0,0,0}},ReqData,State}.

last_modified(ReqData,State) ->
  {calendar:now_to_universal_time(os:timestamp()),ReqData,State}.

generate_etag(ReqData,State) -> {wrq:raw_path(ReqData),ReqData,State}.

hash_body(Body) ->
  mochihex:to_hex(binary_to_list(crypto:hash(sha,Body))).
