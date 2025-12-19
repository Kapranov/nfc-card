-module(multicall).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-export([multicall/3]).

multicall(Pids,What,Timeout) ->
  Parent=self(),
  Caller=proc_lib:spawn(fun() -> multicall_helper(Parent,Pids,What) end),
  Ref=erlang:monitor(process,Caller),
  ok = receive
         {multicall_ready,Caller} -> ok;
         {'DOWN',Ref,process,Caller,normal} -> ok;
         {'DOWN',Ref,process,Caller,Reason} -> {error,{caller_down,Reason}}
       after
         Timeout ->  erlang:exit(Caller,shutdown), ok
       end,
  erlang:demonitor(Ref,[flush]),
  collect_multicall_replies(Pids,[]).

multicall_helper(Parent,Pids,What) ->
  erlang:monitor(process,Parent),
  Refs=send_multicalls(Pids,What,[]),
  collect_multicalls(Parent,Refs).

send_multicalls([],_Request,Refs) ->
  Refs;
send_multicalls([Pid|Pids],What,Refs) ->
  Ref=make_ref(),
  erlang:send(Pid,{'$gen_call',{self(),Ref},What}),
  send_multicalls(Pids,What,[{Ref,Pid}|Refs]).

collect_multicalls(Parent,[]) ->
  Parent ! {multicall_ready,self()};

collect_multicalls(Parent,Refs) ->
  receive
    {'DOWN',_Ref,process,Parent,_Reason} -> ok;
    {Ref,Reply} ->
      Refs1 = case lists:keytake(Ref,1,Refs) of
                false -> Refs;
                {value,{Ref,Pid},Refs1_} ->
                  Parent ! {multicall_reply,Pid,Reply},
                  Refs1_
              end,
      collect_multicalls(Parent, Refs1)
  end.

collect_multicall_replies([],Acc) ->
  lists:reverse(Acc);
collect_multicall_replies([Pid|Pids],Acc) ->
  receive
    {multicall_reply,Pid,Reply} -> collect_multicall_replies(Pids,[{Pid,Reply}|Acc])
  after
    0 -> collect_multicall_replies(Pids,Acc)
  end.
