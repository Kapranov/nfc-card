-ifndef(_BASE_AMQP_HRL_).
-define(_BASE_AMQP_HRL_, true).
-define(HOST,"127.0.0.1").
-define(PASSWORD,<<"guest">>).
-define(PORT,5672).
-define(SERVER,?MODULE).
-define(SSL,none).
-define(TIMEOUT,7_000).
-define(USERNAME,<<"guest">>).
-define(DBG(F, A), io:format("DBG: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(ERR(F, A), io:format("***ERR***: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(INFO(F, A), io:format("===INFO===: ~w:~b: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-record(maui_client,{channel,connection,consumer_tag,continuations=dict:new(),correlation_id=make_ref(),message_id=0,queue}).
-record(base_amqp_deliver, { pid
                             , consumer_tag
                             , delivery_tag
                             , exchange
                             , routing_key
                             , payload
                             , reply_to
                             , correlation_id
                             , message_id
                             }).

-record(base_amqp_ack, {delivery_tag}).
-endif.
