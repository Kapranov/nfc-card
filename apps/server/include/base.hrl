-ifndef(_BASE_AMQP_HRL_).
-define(DBG(F, A), io:format("DBG: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(DEFAULT_TIMEOUT,100).
-define(ERR(F, A), io:format("***ERR***: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(INFO(F, A), io:format("===INFO===: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(RABBIT_CONNECTION_TIMEOUT,7_000).
-define(RABBIT_CONSUMER1,<<"test_bisque1">>).
-define(RABBIT_CONSUMER2,<<"test_bisque2">>).
-define(RABBIT_CONSUMER3,<<"test_bisque3">>).
-define(RABBIT_CONTENT_TYPE,<<"application/json">>).
-define(RABBIT_EXCHANGE1,<<"test_lahaina1">>).
-define(RABBIT_EXCHANGE2,<<"test_lahaina2">>).
-define(RABBIT_EXCHANGE3,<<"test_lahaina3">>).
-define(RABBIT_HOST,"127.0.0.1").
-define(RABBIT_PASSWORD,<<"guest">>).
-define(RABBIT_PAYLOAD_DATA,#{name => <<"Nakamura">>,age => 22,city => <<"Oahu">>}).
-define(RABBIT_PERSISTENT_DELIVERY,2).
-define(RABBIT_PORT,5672).
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
-define(RABBIT_UNKNOWN_KEY,<<"unknown_key">>).
-define(RABBIT_USERNAME,<<"guest">>).
-define(SERVER,?MODULE).
-define(_BASE_AMQP_HRL_,true).

-record(maui_client,{channel :: pid()
                    ,connection :: pid()
                    ,consumer_tag :: string()
                    ,continuations=dict:new()
                    ,correlation_id=make_ref()
                    ,message_id=0
                    ,queue :: string()
                    }).
-record(maui_server,{channel :: pid()
                    ,connection :: pid()
                    ,consumer_count :: integer()
                    ,consumer_tag :: string()
                    ,exchange :: string()
                    ,queue :: string()
                    ,message_count :: integer()
                    ,routing_key :: string()
                    }).
-record(base_amqp_deliver,{pid :: pid()
                          ,consumer_tag :: string()
                          ,delivery_tag :: non_neg_integer()
                          ,exchange :: string()
                          ,routing_key :: string()
                          ,payload :: map()
                          ,reply_to :: string()
                          ,correlation_id :: string()
                          ,message_id :: string()
                          }).

-record(base_amqp_ack, {delivery_tag}).
-endif.
