-module(wakako_config).
-export([dispatch/0,web_config/0]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  {ok,Dispatch}=file:consult(filename:join([filename:dirname(code:which(?MODULE)),"..","priv","dispatch.conf"])),
  lists:flatten([Dispatch]).

web_config() ->
  {ok,App}=application:get_application(?MODULE),
  {ok,IpEnv}=application:get_env(App,web_ip),
  {ok,PortEnv}=application:get_env(App,web_port),
  Ip = case os:getenv("WEBMACHINE_IP") of false -> IpEnv; _ -> IpEnv end,
  Port = case os:getenv("WEBMACHINE_PORT") of false -> PortEnv; _ -> PortEnv end,
  LogDir = case os:getenv("WEBMACHINE_LOG_DIR") of false -> "priv/log"; _ -> "priv/log" end,
  [{ip,Ip},{port,Port},{log_dir,LogDir},{nodelay,true},{dispatch,dispatch()}].
