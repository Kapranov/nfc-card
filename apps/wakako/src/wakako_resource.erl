-module(wakako_resource).
-author('Oleg G.Kapranov <lugatex@yahoo.com>').
-export([init/1]).
-export([service_available/2,allowed_methods/2,content_types_provided/2,content_types_accepted/2,provide_content/2,accept_content/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state,{response=undefined}).

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

service_available(ReqData,State) ->
  {true,ReqData,State}.

allowed_methods(ReqData,State) ->
  Methods=['GET'],
  {Methods,ReqData,State}.

content_types_provided(ReqData,State) ->
  Types=[{"application/json",provide_content}],
  {Types,ReqData,State}.

content_types_accepted(ReqData,State) ->
  Types=[{"application/json",accept_content}],
  {Types,ReqData,State}.

provide_content(ReqData,State) ->
  Content = #{"report" => <<"Hello Wakako, new world">>},
  render_json(Content,ReqData,State).

accept_content(ReqData,State) ->
  io:format("Content ReqData: ~p~n",[ReqData]),
  RawValue = wrq:req_body(ReqData),
  case mochijson2:decode(RawValue) of
    {struct,[{<<"status">>, _Status}]} ->
      {true,ReqData,State#state{response=undefined}};
    _ ->
      {false,ReqData,State#state{response=undefined}}
  end.

render_json(Data,ReqData,State) ->
  Body = mochijson2:encode(Data),
  io:format("Body renderInfo: ~p~n",[ReqData]),
  {Body,ReqData,State}.

%-spec to_html(wrq:reqdata(),term()) -> {iodata(),wrq:reqdata(),term()}.
%to_html(ReqData,State) ->
%  %%%%%%%%%%%%%%%%%%%%%%%
%  %Content = bbmustache:render(<<"{{i}}, {{f}}">>,[{"i", 1}, {"f", 1.5}, {"b", <<"hoge">>}, {"s", "fugo"}, {"a", atom}], [{value_serializer, fun(_X) -> <<"test">> end}]),
%  %Content = bbmustache:render(<<"{{s}} , {{{s}}}">>,[{"s", "A&B"}], [{value_serializer, fun(X) -> "<" ++ X ++ ">" end}]),
%  %Content = bbmustache:render(<<"{{i}}, {{f}}, {{b}}, {{s}}, {{a}}">>,[{"i", 1}, {"f", 1.5}, {"b", <<"hoge">>}, {"s", "fugo"}, {"a", atom}]),
%  %Content = bbmustache:render(<<"hello {{.}}">>, <<"world">>),
%  %Content = bbmustache:render(<<"hello {{.}}">>, "world"),
%  %Content = bbmustache:render(<<"{{.}}">>, 1),
%  %Content = bbmustache:render(<<"{{.}}">>, 1.5),
%  %Content =bbmustache:render(<<"{{.}}">>, atom),
%  %Content = bbmustache:render(<<"{{#.}}{{.}}, {{/.}}">>, [1, 2, 3]),
%  %Content = bbmustache:render(<<"{{.}}">>, #{"a" => "1"}, [{value_serializer, fun(#{"a" := "1"}) -> <<"yes">> end}]),
%  %F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
%  %Content = bbmustache:render(<<"{{#wrapped}}{{name}} is awesome.{{dummy_atom}}{{/wrapped}}">>,[{name, "Willy"}, {wrapped, F}], [{key_type, atom}]),
%  %Content = bbmustache:render(<<"{{ child }}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{{child}}}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{&child}}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{#parent}}{{child}}{{/parent}}">>,[{"parent", true}],[raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{^parent}}{{child}}{{/parent}}">>,[{"parent", false}],[raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{#parent}}{{/parent}}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{^parent}}{{/parent}}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{^parent}}{{child}}{{/parent}}">>,[{"parent", true}],[raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{#parent}}{{child}}{{/parent}}">>,[{"parent", false}],[raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{> not_found_filename}}">>, [], [raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{#parent}}{{ parent . child }}{{/parent}}">>,[{"parent", [{"dummy", true}]}, {"child", []}],[raise_on_context_miss]),
%  %Content = bbmustache:render(<<"{{a}}">>, [{"a", false}]),
%  %Content = bbmustache:render(<<"{{a}}">>, [{"a", null}]),
%  %Content = bbmustache:render(<<"{{a}}">>, [{"a", nil}]),
%  %Content = bbmustache:render(<<"{{#parent}}aaa{{parent.child}}bbb{{/parent}}">>,[{"parent", true}]),
%  %Content = bbmustache:render(<<"{{#parent}}aaa{{parent.child}}bbb{{/parent}}">>,[{"parent", []}],[raise_on_context_miss]),
%  %lists:foreach(fun(X) ->
%  %  Content = bbmustache:render(<<"{{#content}}hello world{{/content}}">>, [{"content", X}]),
%  %  {Content,ReqData,State}
%  %end, ["", <<"">>, nil, false]).
%  %lists:foreach(fun(X) ->
%  %  Content = bbmustache:render(<<"{{^content}}hello world{{/content}}">>, [{"content", X}]),
%  %  {Content,ReqData,State}
%  %end, ["", <<"">>, nil, false]).
%  %Content = bbmustache:render(<<"{{tag}}">>, [{"tag", "value"}], [{escape_fun, fun(X) -> <<"==>", X/binary, "<==">> end}]),
%  %Content = bbmustache:render(<<"<h1>{{title}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsx:encode(X) end}]),
%  %Content = bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsx:encode(X) end}]),
%  %%%%%%%%%%%%%%%%%%%%%%%
%  Content = "<html>Hello Wakako, new world</html>",
%  {Content,ReqData,State}.
%
%to_json(ReqData,State) ->
%  %iolist_to_binary(mochijson2:encode({struct, [{strKey, <<"strVal">>}, {intKey, 10}, {arrayKey, [1, 2, 3]}]})).
%  %mochijson2:decode(<<"{\"strKey\":\"strVal\", \"intKey\":10, \"arrayKey\":[1, 2, 3]}">>).
%  %Content = iolist_to_binary(mochijson2:encode({struct, [{report, <<"Hello Wakako, new world">>}]})),
%  Content = iolist_to_binary(mochijson2:encode(#{"report" => <<"Hello Wakako, new world">>})),
%  {Content,ReqData,State}.
