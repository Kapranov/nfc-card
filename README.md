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
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Talks slow for North Shore first-responder hub, city says">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Wahiawa sex offender to receive sentence in federal child porn case">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Maui County Council member Tasha Kama dies at 73">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Kokua Line: Where can federal workers get relief?">>).
erl> makalapa:send_message(<<"aloha_queue">>,<<"lahaina">>,<<"Honolulu police shut down 3 Kalihi gamerooms, seize $24K in cash">>).
erl> makalapa:start(<<"aloha_queue">>, <<"lahaina">>, <<"fanout">>).
```

### 30 Sep 2025 by Oleg G.Kapranov

[1]: http://127.0.0.1:15672
[2]: http://192.168.2.157:15672
[3]: https://www.rabbitmq.com/docs/management-cli
[4]: https://raw.githubusercontent.com/rabbitmq/rabbitmq-server/v4.1.x/deps/rabbitmq_management/bin/rabbitmqadmin
[5]: https://www.rabbitmq.com/docs/publishers
[6]: https://www.rabbitmq.com/docs/consumers
[7]: https://www.rabbitmq.com/tutorials/tutorial-three-python
