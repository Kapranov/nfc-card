-ifndef(MAUI_SERVER_HRL).
-define(MAUI_SERVER_HRL, true).
-define(DBG(F, A), io:format("DBG: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(DEFAULT_TIMEOUT,100).
-define(ERR(F, A), io:format("***ERR***: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(INFO(F, A), io:format("===INFO===: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(RABBIT_CONNECTION_TIMEOUT,7_000).
-define(RABBIT_CONSUMER1,<<"test_bisque1">>).
-define(RABBIT_CONSUMER2,<<"test_bisque2">>).
-define(RABBIT_CONSUMER3,<<"test_bisque3">>).
-define(RABBIT_CONTENT_TYPE,<<"application/json">>).
-define(RABBIT_CORRELATION_ID,<<"correlation_id">>).
-define(RABBIT_CONTENT_ENCODING,<<"UTF-8">>).
-define(RABBIT_EXCHANGE1,<<"test_lahaina1">>).
-define(RABBIT_EXCHANGE2,<<"test_lahaina2">>).
-define(RABBIT_EXCHANGE3,<<"test_lahaina3">>).
-define(RABBIT_HEARFBEAT,80).
-define(RABBIT_HOST,"127.0.0.1").
-define(RABBIT_PASSWORD,<<"guest">>).
-define(RABBIT_PAYLOAD_DATA,#{name => <<"Nakamura">>,age => 22,city => <<"Oahu">>}).
-define(RABBIT_PERSISTENT_DELIVERY,1).
-define(RABBIT_PORT,5672).
-define(RABBIT_PRIORITY,0).
-define(RABBIT_QUEUE1,<<"test_aloha1">>).
-define(RABBIT_QUEUE2,<<"test_aloha2">>).
-define(RABBIT_QUEUE3,<<"test_aloha3">>).
-define(RABBIT_ROUTING_KEY1,<<"test_mahalo1">>).
-define(RABBIT_ROUTING_KEY2,<<"test_mahalo2">>).
-define(RABBIT_ROUTING_KEY3,<<"test_mahalo3">>).
-define(RABBIT_SSL_OPTIONS,none).
-define(RABBIT_TEST_CUSTOMER1,<<"test_customer_tag1">>).
-define(RABBIT_TEST_CUSTOMER2,<<"test_customer_tag2">>).
-define(RABBIT_TEST_CUSTOMER3,<<"test_customer_tag3">>).
-define(RABBIT_TEST_EXCHANGE1,<<"test_exchange1">>).
-define(RABBIT_TEST_EXCHANGE2,<<"test_exchange2">>).
-define(RABBIT_TEST_EXCHANGE3,<<"test_exchange3">>).
-define(RABBIT_TEST_PAYLOAD1,#{name => <<"Taumua">>,age => 19,city => <<"Kula">>}).
-define(RABBIT_TEST_PAYLOAD2,#{name => <<"Kahele">>,age => 42,city => <<"Hilo">>}).
-define(RABBIT_TEST_PAYLOAD3,#{name => <<"Morikawa">>,age => 56,city => <<"Kokua">>}).
-define(RABBIT_TEST_QUEUE1,<<"test_queue1">>).
-define(RABBIT_TEST_QUEUE2,<<"test_queue2">>).
-define(RABBIT_TEST_QUEUE3,<<"test_queue3">>).
-define(RABBIT_TEST_ROUTING_KEY1,<<"test_routing_key1">>).
-define(RABBIT_TEST_ROUTING_KEY2,<<"test_routing_key2">>).
-define(RABBIT_TEST_ROUTING_KEY3,<<"test_routing_key3">>).
-define(RABBIT_TEST_TYPE,<<"direct">>).
-define(RABBIT_VIRTUAL_HOST,<<"/">>).
-define(RABBIT_UNKNOWN_KEY,<<"unknown_key">>).
-define(RABBIT_UNROUTABLE,<<"unroutable">>).
-define(RABBIT_USERNAME,<<"guest">>).
-define(SERVER,?MODULE).
-record(maui_server,{app_id :: binary()
                    ,channel :: pid()
                    ,cluster_id :: binary()
                    ,connection :: pid()
                    ,consumer_count :: non_neg_integer()
                    ,consumer_tag :: binary()
                    ,content_encoding :: binary()
                    ,content_type :: binary()
                    ,correlation_id :: binary()
                    ,delivery_mode :: non_neg_integer()
                    ,exchange :: binary()
                    ,expiration :: binary()
                    ,headers :: list()
                    ,message_count :: non_neg_integer()
                    ,message_id :: binary()
                    ,priority :: non_neg_integer()
                    ,queue :: binary()
                    ,reply_to :: binary()
                    ,routing_key :: binary()
                    ,timestamp :: non_neg_integer()
                    ,type :: binary()
                    ,uniq :: binary()
                    ,user_id :: binary()
                    }).
-record(consumer_state,{channel
                       ,channel_ref
                       ,consumer
                       ,consumer_ref
                       ,consumer_tag
                       }).
-endif.
