-module(mahalo).
-export([wavenet_test/1]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

-define(HOST,"127.0.0.1").
-define(PASSWORD,<<"guest">>).
-define(PORT,5672).
-define(SSL,none).
-define(TIMEOUT,7_000).
-define(USERNAME,<<"guest">>).

wavenet_test(Val) ->
  {ok,Connection}=amqp_connection:start(amqp_params()),
  {ok,Channel}=amqp_connection:open_channel(Connection),
  BasicPublish=#'basic.publish'{exchange = <<"bisque">>,routing_key = <<"aloha_queue">>},
  ok=amqp_channel:cast(Channel,BasicPublish,#'amqp_msg'{payload=Val}),
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

amqp_params() ->
  #amqp_params_network{
     connection_timeout=?TIMEOUT,
     host=?HOST,
     password=?PASSWORD,
     port=?PORT,
     ssl_options=?SSL,
     username=?USERNAME
  }.
