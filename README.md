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

### 30 Sep 2025 by Oleg G.Kapranov

[1]: http://192.168.2.157:15672
