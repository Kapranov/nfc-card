-module(http_call).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-behaviour(gen_server).

-export([dispatch_request/2]).
-export([amqp_config/0
        ,code_change/3
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,init/1
        ,start_link/3
        ,terminate/2
        ]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-define(SERVER,?MODULE).

-record(state,{channel}).

-spec amqp_config() -> [tuple()].
amqp_config() ->
  {ok,RabbitConnectionTimeout}=application:get_env(server,rabbit_connection_timeout),
  {ok,RabbitHeartbeat}=application:get_env(server,rabbit_heartbeat),
  {ok,RabbitHost}=application:get_env(server,rabbit_host),
  {ok,RabbitPassword}=application:get_env(server,rabbit_password),
  {ok,RabbitPort}=application:get_env(server,rabbit_port),
  {ok,RabbitSSLOptions}=application:get_env(server,rabbit_ssl_options),
  {ok,RabbitUsername}=application:get_env(server,rabbit_username),
  {ok,RabbitVirtualHost}=application:get_env(server,rabbit_virtual_host),
  [{connection_timeout,RabbitConnectionTimeout}
  ,{heartbeat,RabbitHeartbeat}
  ,{host,RabbitHost}
  ,{password,RabbitPassword}
  ,{port,RabbitPort}
  ,{ssl_options,RabbitSSLOptions}
  ,{username,RabbitUsername}
  ,{virtual_host,RabbitVirtualHost}
  ].

dispatch_request(_,Req) ->
  gen_server:call({local,?SERVER},{request,Req}),
  ok.

start_link(Config,Exchange,Type) when is_list(Config);
                                      is_binary(Exchange);
                                      is_binary(Type) ->
  {ok,Pid}=gen_server:start_link({local,?SERVER},?SERVER,[Config,Exchange,Type],[]),
  io:format("HTTP Server started with pid: ~p~n",[Pid]),
  {ok,Pid}.

init([Config,Name,Type]) ->
  {ok,Channel}=http_util:start_rabbitmq_channel(Config,Name,Type),
  {ok,#state{channel=Channel}}.

handle_call({request,Req},_,State=#state{channel=Channel}) ->
  Method=Req:get(method),
  Path=Req:get(raw_path),
  Body=Req:recv_body(),
  Response=handle_request(Method,Path,Body,Channel),
  Req:respond(Response),
  Req:cleanup(),
  {reply,ok,State};
handle_call(_Msg,_From,State) -> {reply,ok,State}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,#state{channel=Channel}) ->
  amqp_connection:close(Channel),
  ok;
terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

handle_request('GET',"/http/api/test",_,Channel) ->
  publish_message(<<"test">>,"test",Channel).

publish_message(_,_,_) ->
  {404,[{"Content-Type","text/plain"}],<<"Message Not Defined">>}.
