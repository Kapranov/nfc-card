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

### 30 Sep 2025 by Oleg G.Kapranov

[1]: http://127.0.0.1:15672
[2]: http://192.168.2.157:15672
[3]: https://www.rabbitmq.com/docs/management-cli
[4]: https://raw.githubusercontent.com/rabbitmq/rabbitmq-server/v4.1.x/deps/rabbitmq_management/bin/rabbitmqadmin
[5]: https://www.rabbitmq.com/docs/publishers
[6]: https://www.rabbitmq.com/docs/consumers
[7]: https://www.rabbitmq.com/tutorials/tutorial-three-python
