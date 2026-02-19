nfc_card
=====

```
bash> rebar3 new release nfc_card
bash> rebar3 shell
```

## App `Core` - DB models

## App `Server` - HTTP server

## Мікросервіс обліку часу роботи співробітників

Мікросервіс потрібен для обліку часу роботи співробітників за допомогою NFC карток.
NFC картка не є пропуском, а використовується лише для ідентифікації співробітника.
Співробітник має зареєструвати картку лише двічі вдень: коли прийшов на роботу та
коли пішов з роботи.
Адміністратор має мати можливість подивитись історію, ввести графік роботи та
додати виключення в графік: вихідний, дозволене запізнення чи передчасне покидання
роботи.

Реалізувати сервіс, який виступає в ролі `RPC`-сервера, що обробляє запити через
`RabbitMQ`. Основні вимоги:

- `Elixir/Erlang`
- `RPC-server` via `RabbitMQ`
- `PostgreSQL`
- для всіх методів:
    - request `Content-type: application/json`
    - response `Content-type: application/json`
    - валідація вхідних даних

Нижче список всіх необхідних методів, які потрібно реалізувати.

`/card/touch`
Обов’язкові поля: "card_uid"
Реєстрація картки в системі (робітник “пропікав” картку в зчитувачі)
**Example response**: `{card_uid: string, user_id: number}`

`/card/assign`
Обов’язкові поля: "user_id", "card_uid"
Прив’язка картки до робітника
**Example response**: `{card_uid: string, user_id: number}`

`/card/delete`
Обов’язкові поля: "card_uid"
Видалення картки
**Example response**: `{card_uid: string, user_id: number}`

`/card/list_by_user`
Обов’язкові поля: "user_id"
Отримати список всіх карток, що прив’язані до робітника
**Example response**: `{user_id: Array<card_uid>}`

`/card/delete_all_by_user`
Обов’язкові поля: "user_id"
Видалення всіх карток, що прив’язані до робітника
**Example response**: `{user_id: Array<card_uid>}`

`/work_time/set`
Обов’язкові поля: "user_id", "start_time", "end_time", "days"
Задання робочого часу робітника

`/work_time/get`
Обов’язкові поля: "user_id"
Отримання робочого часу робітника

`/work_time/add_exclusion`
Обов’язкові поля: "user_id", "type_exclusion", “start_datetime”, “end_datetime”
`type_exclusion` - прийти пізніше, піти раніше, повний робочий день
Додати виключення в робочий графік

`/work_time/get_exclusion`
Обов’язкові поля: "user_id"
Отримати всі виключення з робочого графіку

`/work_time/history_by_user`
Обов’язкові поля: "user_id"
Отримати історії робітника

`/work_time/statistics_by_user`
Обов’язкові поля: "user_id"
Також можливість фільтрації даних тиждень, місяць, рік, весь період. Default: місяць
Отримати статистику робітника
Скільки повинен працювати, скільки пропрацював часу, скільки не допрацював часу,
скільки разів запізнювався (без причини/по причині), скільки разів йшов раніше (без
причини/по причині)

### Usage RabbitMQ

```
bash> sudo rabbitmqctl status
bash> sudo rabbitmq-diagnostics status
bash> sudo rabbitmq-diagnostics environment | grep disk_free_limit
bash> sudo rabbitmqctl set_disk_free_limit 1000000000
bash> rabbitmqctl deactivate_free_disk_space_monitoring
bash> sudo systemctl stop rabbitmq
bash> sudo systemctl start rabbitmq
bash> sudo systemctl enable rabbitmq
bash> sudo systemctl disable rabbitmq

# disk_free_limit.absolute = 1GB
# disk_free_limit.absolute = 1000000000

bash> makalapa:start(<<"aloha_queue">>).
bash> makalapa:send_messages(<<"aloha_queue">>, <<"Japanese archery tradition finds home in Kaimuki">>).
bash> makalapa:send_messages(<<"aloha_queue">>, <<"HECO proposes backup power rebate program">>).
bash> rabbitmqadmin get queue='aloha_queue' count=2
```

```
erl> makalapa:start(<<"aloha_queue">>,<<"lahaina">>,<<"fanout">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Talks slow for North Shore first-responder hub, city says">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Wahiawa sex offender to receive sentence in federal child porn case">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Maui County Council member Tasha Kama dies at 73">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Kokua Line: Where can federal workers get relief?">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Honolulu police shut down 3 Kalihi gamerooms, seize $24K in cash">>).
```

```
erl> maui:start_link(<<"lahaina">>,<<"aloha_queue">>,<<"fanout">>,<<"aloha_queue">>,<<"bisque">>).
erl> maui:publish(<<"Honolulu police shut down 3 Kalihi gamerooms, seize $24K in cash">>).
erl> maui:publish(<<"Talks slow for North Shore first-responder hub, city says">>).
erl> maui:publish(<<"Wahiawa sex offender to receive sentence in federal child porn case">>).
erl> maui:publish(<<"Maui County Council member Tasha Kama dies at 73">>).
erl> maui:publish(<<"Kokua Line: Where can federal workers get relief?">>).
erl> maui:publish(<<"Honolulu police shut down 3 Kalihi gamerooms, seize $24K in cash">>).
```

```
erl> {ok,Brokers}=application:get_env(server,brokers).
erl> Rabbit=proplists:get_value(rabbit,Brokers).
erl> F = fun(K) -> proplists:get_value(K,Rabbit) end.
erl> F(connection_timeout).
erl> F(consumer1).
erl> F(consumer2).
erl> F(consumer3).
erl> F(content_type).
erl> F(exchange1).
erl> F(exchange2).
erl> F(exchange3).
erl> F(host).
erl> F(node).
erl> F(password).
erl> F(payload).
erl> F(persistent_delivery).
erl> F(port).
erl> F(queue1).
erl> F(queue2).
erl> F(queue3).
erl> F(routing_key1).
erl> F(routing_key2).
erl> F(routing_key3).
erl> F(ssl).
erl> F(test_exchange1).
erl> F(test_exchange2).
erl> F(test_exchange3).
erl> F(test_payload1).
erl> F(test_payload3).
erl> F(test_queue1).
erl> F(test_queue2).
erl> F(test_queue3).
erl> F(test_routing_key1).
erl> F(test_routing_key2).
erl> F(test_routing_key3).
erl> F(test_type).
erl> F(type).
erl> F(unknown_key).
erl> F(username).
erl> F({test_payload2).
```

### For example usage MauiServer commands:

```
bash> make test
bash> make run
bash> rabbitmqadmin get queue='test_aloha1' count=2
bash> rabbitmqadmin get queue='client.fanout.bPB17Qs1+1oxZCYPzzk2Rg==' count=2
```

```
erl> Exchange = <<"test_lahaina1">>.
erl> Queue = <<"test_aloha1">>.
erl> Type = <<"fanout">>.
erl> RoutingKey = <<"test_mahalo1">>.
erl> ConsumerTag = <<"test_bisque1">>.
erl> Args = [{connection_timeout,7000},{heartbeat,80},{host,"127.0.0.1"},{password,<<"guest">>},{port,5672},{ssl_options,none},{ssl_options,none},{username,<<"guest">>},{virtual_host,<<"/">>}].
erl> Conf = maui_server:amqp_config().
erl> Mssg = #{age=>99,city=>"Honolulu",name=>"Kaplanov"}.
erl> {ok,DeliveryTag} = application:get_env(server,rabbit_persistent_delivery).
erl> {ok,Connection} = amqp_connection:start(maui_server:amqp_params(maui_server:amqp_args(Args))).
erl> {ok,Channel} = amqp_connection:open_channel(Connection).
erl> maui_server:ack_message(Channel,DeliveryTag).
erl> maui_server:amqp_params(Conf).
erl> maui_server:amqp_params(maui_server:amqp_args(Args)).
erl> maui_server:basic_cancel(Channel,ConsumerTag).
erl> maui_server:consume(Channel,Queue).
erl> maui_server:consumer_start(Channel,Queue,self()).
erl> maui_server:consumer_init(Channel,Queue,ConsumerTag).
erl> maui_server:generate_msg_id().
erl> maui_server:off().
erl> maui_server:exchange(Channel,Exchange,Type).
erl> maui_server:queue(Queue).
erl> maui_server:bind(Channel,Exchange,Queue,RoutingKey).
erl> maui_server:unbind(Channel,Exchange,Queue,RoutingKey).
erl> maui_server:delete_exchange(Channel,exchange,Exchange).
erl> maui_server:delete_queue(Channel,queue,Queue).
erl> maui_server:publish(Mssg).
erl> maui_server:start().
erl> maui_server:start_link(Args,Exchange,Queue,Type,RoutingKey,ConsumerTag).
erl> maui_server:stop().
erl> maui_server:time_since_epoch().
```

### For example usage MauiClient commands:

```
erl> Queue = <<"test_aloha1">>.
erl> ConsumerTag = <<"test_bisque1">>.
erl> Args = [{connection_timeout,7000},{heartbeat,80},{host,"127.0.0.1"},{password,<<"guest">>},{port,5672},{ssl_options,none},{ssl_options,none},{username,<<"guest">>},{virtual_host,<<"/">>}].
erl> Conf = maui_client:amqp_config().
erl> maui_client:amqp_args(Args).
erl> maui_client:amqp_params(Conf).
erl> maui_client:amqp_params(maui_client:amqp_args(Args)).
erl> maui_client:binary(none).
erl> maui_client:binary(<<"Aloha!">>).
erl> maui_client:binary([<<"Aloha!">>]).
erl> maui_client:off().
erl> maui_client:ref_to_string().
erl> maui_client:start().
erl> maui_client:fetch().
erl> maui_client:start_link(Args,Queue,ConsumerTag).
erl> maui_client:stop().
erl> maui_client:uuid().
```

```
    %amqp_channel:cast(State#state.channel, #'basic.publish'{exchange = ?EVENTS_EXCHANGE}, #amqp_msg{props = #'P_basic'{}, payload = Message}),
    %after 4000 -> io:format("~n"), io:format("Message timeout exceeded ~n")
    %Any -> io:format("received unexpected Any: ~p~n",[Any]), ttt(Channel)
    %Now = date_utils:now_to_milliseconds_hires(erlang:now()),
    %#amqp_msg{props = Props, payload = term_to_binary(Payload)}),
```

### Erlang and Kafka

Example of a configuration file (for `sys.config`):

```erlang
{erlkaf, [

    {global_client_options, [
        {bootstrap_servers, <<"broker1.com:9092,broker2.com:9092">>},
    ]},

    {clients, [
        {client_producer_id, [

            {type, producer},

            {topics, [
                {<<"benchmark">>, [{request_required_acks, 1}]}
            ]},

            {client_options, [
                {queue_buffering_max_messages, 10000}
            ]}
        ]},

        {client_consumer_id, [

            {type, consumer},

            {group_id, <<"erlkaf_consumer">>},
            {topics, [
                {<<"benchmark">>, [
                    {callback_module, module_topic1},
                    {callback_args, []},
                    {dispatch_mode, one_by_one}
                ]}
            ]},
            {topic_options, [
                {auto_offset_reset, smallest}
            ]},

            {client_options, [
                {offset_store_method, broker}
            ]}
        ]}
    ]}
]}
```

`global_client_options` will apply to all clients defined. In case the global property it's defined as well in the client
options it's value will be overwritten.

For producers in case you don't need to customize the topic properties you can omit the `topics` property as time they will
be created on the first produce operation with default settings.


### HTTP API

The code snippet `Path = Req:get(raw_path)` is an Erlang expression commonly used within web frameworks
like MochiWeb, Cowboy, or Elli to extract the raw, undecoded path from an HTTP request object (usually named Req).

This is a standard pattern for accessing request information within these frameworks.

- `Req` is a variable representing the HTTP request object, which is typically passed into handler functions.
- `:get(raw_path)` is a function call made on the Req object (using Erlang's object-like call syntax for records
  or parameterized modules) to retrieve the value associated with the `raw_path` field or key.
- `raw_path` refers to the original, URL-encoded path component of the request URL, including the query string if present.

Example Usage in `MochiWeb`

A typical use case within a `MochiWeb` application's `loop/1` function might look like this:

```erlang
loop(Req) ->
    RawPath = Req:get(raw_path), % Get the full raw path, e.g., <<"/some/page?id=1">>
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath), % Extract just the path part, e.g., <<"/some/page">>
    case Path of
        <<"/">> ->
            respond(Req, <<"Hello World!">>);
        _ ->
            respond(Req, <<"Page not found">>)
    end.
```

### MochiWeb

```
bash> openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout ~/api_server/cert/cert.key -out ~/api_server/cert/cert.crt
```

```erlang
erl> mochiweb_base64url:decode(mochiweb_base64url:encode(Path)).

erl> ServerFun = fun() -> [] end.
erl> Opts = [].
erl> ServerOpts = [{ip, "127.0.0.1"}, {port, 4999}, {backlog, 5}, {loop, ServerFun}].
erl> {ok, Server} = mochiweb_socket_server:start(ServerOpts ++ Opts).
erl> Port = mochiweb_socket_server:get(Server, port).
erl> mochiweb_socket_server:stop(Server).


erl> Headers = mochiweb_headers:make([{"Accept", "text/html"}]).
erl> Headers = mochiweb_headers:make([{"content-type", "application/json"}, {"date", "Sun, 11 Jan 2026 05:07:19"}]).
erl> mochiweb_headers:get_value(K, Headers).
erl> mochiweb_headers:insert(Header, Value, Headers)).

erl> {ok, Socket} = gen_tcp:listen(0, [{active, false}]).
erl> Req = mochiweb_request:new(Socket, 'GET', "/foo", {1, 1}, Headers).
erl> mochiweb_request:get(path,Req).
erl> mochiweb_socket:close(Socket).
erl> gen_tcp:close(Socket).

erl> ReplyPrefix = "You requested: ".
erl> ExHeaders = [{"Set-Cookie", "foo=bar"}, {"Set-Cookie", "foo=baz"}],
erl> ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ mochiweb_request:get(path, Req),
                        mochiweb_request:ok({"text/plain", ExHeaders, Reply}, Req)
                    end.

erl> ServerFun = fun (Req) ->
                        Body = mochiweb_request:recv_body(Req),
                        Headers = [{"Content-Type", "application/octet-stream"}],
                        mochiweb_request:respond({201, Headers, Body}, Req)
                    end,

erl> Req = mochiweb_request:new(nil, 'GET', "/foo", {1, 1}, mochiweb_headers:make([{"Content-Type","application/json"}])).

erl> Headers = mochiweb_headers:make([{"Content-type", "application/json"},{'Content-Length', "0"}]).
erl> ContentLength = list_to_integer(mochiweb_headers:get_value("Content-Length", Headers)).
erl> Req = mochiweb_request:new(testing, [{recv_body, <<"Hello world!">>}], 'GET', "/foo", {1, 1}, Headers).
erl> mochiweb_request:get(method,Req).
erl> mochiweb_request:get(opts,Req).
erl> mochiweb_request:get(path,Req).
erl> mochiweb_request:get(raw_path,Req).
erl> mochiweb_request:get(headers,Req).
erl> mochiweb_request:recv_body(Req).

bash> openssl req -newkey rsa:2048 -nodes -x509 -subj '/CN=name-you-want.example.com' -days 3650 -out server.cert -keyout server.key
bash> openssl s_server -accept 7781 -cert server.cert -key server.key -WWW

erl> inets:start(httpc, [{profile, my_client_profile}]).
erl> httpc:request(get, {Url, []}, [], []).
erl> httpc:get_options(all).

bash> erl -S httpd
bash> erl -S httpd serve
bash> erl -S httpd serve path/to/dir
bash> erl -s inets -eval 'inets:start(httpd,[{server_name,"127.0.0.1"},{document_root, "."},{server_root, "."},{port, 8080},{mime_types,[{"html","text/html"},{"htm","text/html"}]}]).'
bash> w3m http://localhost:8080

erl> application:start(inets).
erl> Path = "/home/kapranov/Projects/bin/erlang-projects/erlang-rabbitmq/mochiweb/examples/example_project/priv/www".
erl> inets:start(httpd,[{server_name,"localhost"},{document_root, Path},{server_root, Path},{port, 8080},{mime_types,[{"html","text/html"},{"htm","text/html"}]}]).
erl> httpc:request("http://localhost:8080").
erl> httpc:request(get, {"http://localhost:8080", []}, [], []).
erl> Headers = [{"content-type", "text/plain"}].
erl> Headers = [{"content-type", "text/html"}].
erl> Headers = [{"content-type", "application/json"}].
erl> httpc:request(get, {"http://localhost:8080", Headers}, [], []).
erl> httpc:request(get, {"http://localhost:8080", []}, [{"content-type", "text/plain"}], []).
erl> application:stop(inets).

erl> Opts = [{name,testing},{port,8080},{path,"http://localhost/api"}].
erl> kilauea:start(Opts).
erl> httpc:request("http://localhost:8080/").
erl> httpc:request("http://localhost:8080/proba").
erl> httpc:request("http://localhost:8080/hello_world").
erl> kilauea:request(get,"http://localhost:8080/").
erl> kilauea:request(get,"http://localhost:8080/proba").
erl> kilauea:request(get,"http://localhost:8080/hello_world").


erl> Profile = Profile = [{name, http_call}, {port, 8080}, {active_sockets, false}, {timing, 3000}].
erl> Opts = [{name,http_call},{port,8080},{ip,"127.0.0.1"},{backlog, 10},{nodelay, true},{acceptor_pool_size, 10}, {ssl, false},{profile_fun, fun () -> Profile end},{link, false}, {recbuf, 1}].
erl> http_call:start(Opts).
erl> http_call:retrieve(get, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}]).
erl> http_call:retrieve(get, "http://localhost:8080/hello_world", [{"Content-Type","text/plain"}]).
erl> http_call:retrieve(get, "http://localhost:8080/hello", [{"Content-Type","application/json"}]).
erl> http_call:request(get, "http://localhost:8080/hello_world").
erl> http_call:request(get, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}]).
erl> http_call:request(get, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}], "application/json", "Aloha").
erl> http_call:request(put, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}], "application/json", "Aloha").
erl> http_call:request(post, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}], "application/json", "Aloha").
erl> http_call:request(delete, "http://localhost:8080/hello_world", [{"Content-Type","application/json"}], "application/json", "Aloha").

erl> Hexkey = mochihex:to_hex(crypto:strong_rand_bytes(8)).
erl> Req = mochiweb_request:new(nil,'GET',"/foo",{1, 1},mochiweb_headers:make([{"Sec-WebSocket-Key","Xn3fdKyc3qEXPuj2A3O+ZA=="}])).
erl> SecKey = mochiweb_request:get_header_value("sec-websocket-key",Req).
erl> BinKey = list_to_binary(SecKey).
erl> Bin = <<BinKey/binary,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>.
erl> Challenge = base64:encode(crypto:hash(sha,Bin)).
erl> Headers = [{"Content-Type","application/octet-stream"},{"Sec-Websocket-Accept",Challenge}].
erl> SecKey =/= undefined.
erl> if SecKey =/= undefined -> hybi_handshake(SecKey);
           Sec1Key =/= undefined andalso Sec2Key =/= undefined ->
             Host = ReqM:get_header_value("Host", Req),
             Path = ReqM:get(path, Req),
             Body = ReqM:recv(8, Req),
             Scheme = scheme(Req),
             hixie_handshake(Scheme, Host, Path, Sec1Key, Sec2Key, Body, Origin);
           true -> error
        end.

hixie_handshake(Scheme,Host,Path,Key1,Key2,Body,Origin) ->
  Location = lists:concat([Scheme,Host,Path]),
  Challenge = erlang:md5(Ckey),
  Response = {101,[{"Upgrade","WebSocket"},{"Connection","Upgrade"},{"Sec-WebSocket-Origin",Origin},{"Sec-WebSocket-Location",Location}],Challenge},
  {hixie, Response}.

ServerFun = fun (Req) ->
    Body = mochiweb_request:recv_body(Req),
    Headers = [{"Content-Type", "application/octet-stream"}],
    mochiweb_request:respond({201, Headers, Body}, Req)
end.
```

In the example above, `mochiweb_util:urlsplit_path/1` is used to separate
the path from the query string for routing purposes.

### 30 Sep 2025 by Oleg G.Kapranov

[1]:  http://127.0.0.1:15672
[2]:  http://192.168.2.157:15672
[3]:  https://www.rabbitmq.com/docs/management-cli
[4]:  https://raw.githubusercontent.com/rabbitmq/rabbitmq-server/v4.1.x/deps/rabbitmq_management/bin/rabbitmqadmin
[5]:  https://www.rabbitmq.com/docs/publishers
[6]:  https://www.rabbitmq.com/docs/consumers
[7]:  https://www.rabbitmq.com/tutorials/tutorial-three-python
[8]:  https://github.com/silviucpp/erlkaf
[9]:  https://github.com/kafka4beam/brod
[10]: https://github.com/erleans/vonnegut
[11]: https://github.com/BerkOzdilek/emq_kafka_bridge
[12]: https://github.com/HCA-Healthcare/brod_oauth
[13]: https://github.com/NimsHub/Kafka-with-Erlang
[14]: https://daemon.pizza/posts/erlang-httpc/
[15]: https://www.proctor-it.com/erlang-thursday-httpc-request-1-and-httpc-request-4/
[16]: https://medium.com/@agus81/erlang-is-fun-series-application-server-on-mochiweb-part-2-faae9e992464
[17]: https://elixirforum.com/t/httpc-cheatsheet/50337
[18]: https://erlangforums.com/t/is-httpc-considered-bad-why-shouldn-t-i-use-it/3505
[19]: https://erlangforums.com/t/httpc-httpd-improvements/2622
[20]: https://gist.github.com/derdesign/4598832
[21]: https://gist.github.com/willurd/5720255#erlang
[22]: https://github.com/jkvor/hello-erlang/tree/master
[23]: https://github.com/tsloughter/reqerl
[24]: https://github.com/erlang/otp/issues/5074
[25]: https://github.com/erlang/otp/pull/7299
[26]: https://github.com/erlang/otp/pull/9473
[27]: https://github.com/erlang/otp/pull/9473/commits/12e9c89cf851c6e275ffb54f6d7b13c1ba25b04b
[28]: https://github.com/erlang/otp/blob/master/lib/inets/src/http_client/httpc.erl
[29]: https://www.erlang.org/doc/apps/inets/httpc.html
[30]: https://habr.com/ru/articles/111600/
[31]: https://habr.com/ru/articles/111350/
[32]: https://habr.com/ru/articles/111252/
[33]: https://github.com/hexedpackets/trot
[34]: https://github.com/benoitc/mochicow
[35]: https://git.sr.ht/~fancycade/nine_cowboy/tree/main/item/src/nine_cowboy_mid.erl
[36]: https://github.com/inaka/shotgun
[37]: https://github.com/esl/fusco
