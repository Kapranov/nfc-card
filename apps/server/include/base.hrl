-ifndef(_BASE_AMQP_HRL_).
-define(CONSUMER1,<<"test_bisque1">>).
-define(CONSUMER2,<<"test_bisque2">>).
-define(CONSUMER3,<<"test_bisque3">>).
-define(CONTENT_TYPE,<<"application/json">>).
-define(DBG(F, A), io:format("DBG: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(ERR(F, A), io:format("***ERR***: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(EXCHANGE1,<<"test_lahaina1">>).
-define(EXCHANGE2,<<"test_lahaina2">>).
-define(EXCHANGE3,<<"test_lahaina3">>).
-define(HOST,"127.0.0.1").
-define(INFO(F, A), io:format("===INFO===: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(MESSAGE,#{name => <<"Nakamura">>,age => 22,city => <<"Oahu">>}).
-define(PASSWORD,<<"guest">>).
-define(PERSISTENT_DELIVERY,2).
-define(PORT,5672).
-define(QUEUE1,<<"test_aloha1">>).
-define(QUEUE2,<<"test_aloha2">>).
-define(QUEUE3,<<"test_aloha3">>).
-define(ROUTING_KEY1,<<"test_mahalo1">>).
-define(ROUTING_KEY2,<<"test_mahalo2">>).
-define(ROUTING_KEY3,<<"test_mahalo3">>).
-define(SERVER,?MODULE).
-define(SSL,none).
-define(TIMEOUT,7_000).
-define(UNKNOWN_KEY,<<"unknown_key">>).
-define(USERNAME,<<"guest">>).
-define(_BASE_AMQP_HRL_, true).

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
