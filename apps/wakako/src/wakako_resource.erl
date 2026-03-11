-module(wakako_resource).
-export([init/1,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok,term()}.
init([]) -> {ok,undefined}.

-spec to_html(wrq:reqdata(),term()) -> {iodata(),wrq:reqdata(),term()}.
to_html(ReqData,State) ->
  Content = bbmustache:render(<<"<html>{{{report}}}</html>">>, #{"report" => "Hello Wakako, new world"}),
  %%%%%%%%%%%%%%%%%%%%%%%
  %Content = bbmustache:render(<<"{{i}}, {{f}}">>,[{"i", 1}, {"f", 1.5}, {"b", <<"hoge">>}, {"s", "fugo"}, {"a", atom}], [{value_serializer, fun(_X) -> <<"test">> end}]),
  %Content = bbmustache:render(<<"{{s}} , {{{s}}}">>,[{"s", "A&B"}], [{value_serializer, fun(X) -> "<" ++ X ++ ">" end}]),
  %Content = bbmustache:render(<<"{{i}}, {{f}}, {{b}}, {{s}}, {{a}}">>,[{"i", 1}, {"f", 1.5}, {"b", <<"hoge">>}, {"s", "fugo"}, {"a", atom}]),
  %Content = bbmustache:render(<<"hello {{.}}">>, <<"world">>),
  %Content = bbmustache:render(<<"hello {{.}}">>, "world"),
  %Content = bbmustache:render(<<"{{.}}">>, 1),
  %Content = bbmustache:render(<<"{{.}}">>, 1.5),
  %Content =bbmustache:render(<<"{{.}}">>, atom),
  %Content = bbmustache:render(<<"{{#.}}{{.}}, {{/.}}">>, [1, 2, 3]),
  %Content = bbmustache:render(<<"{{.}}">>, #{"a" => "1"}, [{value_serializer, fun(#{"a" := "1"}) -> <<"yes">> end}]),
  %F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
  %Content = bbmustache:render(<<"{{#wrapped}}{{name}} is awesome.{{dummy_atom}}{{/wrapped}}">>,[{name, "Willy"}, {wrapped, F}], [{key_type, atom}]),
  %Content = bbmustache:render(<<"{{ child }}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{{child}}}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{&child}}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{#parent}}{{child}}{{/parent}}">>,[{"parent", true}],[raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{^parent}}{{child}}{{/parent}}">>,[{"parent", false}],[raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{#parent}}{{/parent}}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{^parent}}{{/parent}}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{^parent}}{{child}}{{/parent}}">>,[{"parent", true}],[raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{#parent}}{{child}}{{/parent}}">>,[{"parent", false}],[raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{> not_found_filename}}">>, [], [raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{#parent}}{{ parent . child }}{{/parent}}">>,[{"parent", [{"dummy", true}]}, {"child", []}],[raise_on_context_miss]),
  %Content = bbmustache:render(<<"{{a}}">>, [{"a", false}]),
  %Content = bbmustache:render(<<"{{a}}">>, [{"a", null}]),
  %Content = bbmustache:render(<<"{{a}}">>, [{"a", nil}]),
  %Content = bbmustache:render(<<"{{#parent}}aaa{{parent.child}}bbb{{/parent}}">>,[{"parent", true}]),
  %Content = bbmustache:render(<<"{{#parent}}aaa{{parent.child}}bbb{{/parent}}">>,[{"parent", []}],[raise_on_context_miss]),
  %lists:foreach(fun(X) ->
  %  Content = bbmustache:render(<<"{{#content}}hello world{{/content}}">>, [{"content", X}]),
  %  {Content,ReqData,State}
  %end, ["", <<"">>, nil, false]).
  %lists:foreach(fun(X) ->
  %  Content = bbmustache:render(<<"{{^content}}hello world{{/content}}">>, [{"content", X}]),
  %  {Content,ReqData,State}
  %end, ["", <<"">>, nil, false]).
  %Content = bbmustache:render(<<"{{tag}}">>, [{"tag", "value"}], [{escape_fun, fun(X) -> <<"==>", X/binary, "<==">> end}]),
  %Content = bbmustache:render(<<"<h1>{{title}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsx:encode(X) end}]),
  %Content = bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsx:encode(X) end}]),
  %%%%%%%%%%%%%%%%%%%%%%%
  {Content,ReqData,State}.
