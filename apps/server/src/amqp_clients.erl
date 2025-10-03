-module(amqp_clients).

-export([start/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

start() ->
  {ok, Conn} = amqp_connection:start(#amqp_params_network{host = "127.0.0.1", username = <<"kapranov">>,  password = <<"kapranov">>}),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"my_queue">>}),
  amqp_channel:cast(Channel, #'basic.publish'{exchange = <<"">>, routing_key = <<"my_queue">>}, #amqp_msg{payload = <<"Hello, RabbitMQ!">>}),
  amqp_channel:close(Channel),
  amqp_connection:close(Conn),
  ok.

amqp_params() ->
   #amqp_params_network{
      username = "kapranov",
      password = "kapranov",
      port = 5672,
      host = localhost,
      connection_timeout = 7000,
      ssl_options = none
   }.
