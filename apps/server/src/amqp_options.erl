-module(amqp_options).

-export([parse/1, parse/2]).

-include("./_build/default/lib/amqp_client/include/amqp_client.hrl").

parse(Opts) when is_record(Opts, amqp_params_network) ->
  Opts;
parse(Opts) ->
  parse(Opts, #{}).

parse([], Acc) -> amqp_params(maps:to_list(Acc));
parse(Opts, Acc) when is_map(Opts) ->
   parse(maps:to_list(Opts), Acc);
parse([{host, Host} | R], Acc) when is_binary(Host) ->
   parse(R, Acc#{host => binary_to_list(Host)});
parse([{host, Host} | R], Acc) when is_list(Host) ->
   parse(R, Acc#{host => Host});
parse([{port, Port} | R], Acc) when is_integer(Port) ->
   parse(R, Acc#{port => Port});
parse([{heartbeat, KeepAlive} | R], Acc) when is_integer(KeepAlive) ->
   parse(R, Acc#{heartbeat => KeepAlive});
parse([{user, User} | R], Acc) when is_binary(User) ->
   parse(R, Acc#{user => User});
parse([{pass, Pass} | R], Acc) when is_binary(Pass) ->
   parse(R, Acc#{pass => Pass});
parse([{vhost, VHost} | R], Acc) when is_binary(VHost) ->
   parse(R, Acc#{vhost => VHost});
parse([{connection_timeout, ConnTimeout} | R], Acc) when is_integer(ConnTimeout) ->
   parse(R, Acc#{connection_timeout => ConnTimeout});
parse([{ssl, false} | R], Acc) ->
   parse(R, Acc#{ssl => false});
parse([_ | R], Acc) ->
   parse(R, Acc).

amqp_params(Config) ->
   #amqp_params_network{connection_timeout=proplists:get_value(connection_timeout,Config,7_000)
                       ,heartbeat=proplists:get_value(heartbeat,Config,80)
                       ,host=proplists:get_value(host,Config)
                       ,password=proplists:get_value(password,Config,<<"guest">>)
                       ,port=proplists:get_value(port,Config)
                       ,ssl_options=proplists:get_value(ssl_options,Config,none)
                       ,username=proplists:get_value(user,Config,<<"guest">>)
                       ,virtual_host=proplists:get_value(virtual_host,Config,<<"/">>)
                       }.
