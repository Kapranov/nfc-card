-module(amqp_clients).

-export([start/0, amqp_params/0, read_messages/0]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

start() ->
  {ok, Conn} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"aloha_queue">>}),
  amqp_channel:cast(Channel, #'basic.publish'{exchange = <<"">>, routing_key = <<"aloha_queue">>}, #amqp_msg{payload = <<"Aloha, NFC-card!">>}),
  amqp_channel:close(Channel),
  amqp_connection:close(Conn),
  ok.

amqp_params() ->
   #amqp_params_network{
      username = <<"guest">>,
      password = <<"guest">>,
      port = 5672,
      host = "127.0.0.1",
      connection_timeout = 7000,
      ssl_options = none
   }.

read_messages() ->
  {ok, Conn} = amqp_connection:start(amqp_params()),
  {ok, Channel} = amqp_connection:open_channel(Conn),
  Declare = #'queue.declare'{queue = <<"my_queue">>},
  #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
  #'basic.consume_ok'{consumer_tag = _Tag} =
      amqp_channel:call(Channel, #'basic.consume'{queue = <<"my_queue">>}),
  loop(Channel),
  ok.

loop(Channel) ->
  receive
    #'basic.consume_ok'{} ->
        loop(Channel);
    #'basic.cancel_ok'{} ->
        ok;
    {#'basic.deliver'{delivery_tag = Tag}, Content} ->
        R = io_lib:format("~p", [Content]),
        lists:flatten(R),
        io:fwrite("Received\n"),
        io:fwrite(R),
        amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
        loop(Channel)
  end.
