-module(gms3).
-export([start/1,start/2]).
-define(arghh, 300).
-define(timeout, 300).

start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  rand:seed(exsss, {Rnd, Rnd, Rnd}),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
      Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2); 
    stop ->
      io:format("Leader: ~w (~w) stops~n",[Id,self()]),
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);


    {msg, I, _} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, I + 1, {msg, I, Msg}, Slaves, Group);


    {view, I, _, _} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, I, [Leader|Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, I, {view, I+1, [Leader|Slaves2],Group2}, Slaves2, Group2);

    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok 
  end.

bcast(ID,Msg,Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(ID) end, Nodes).

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("Leader : ~w~n",[Id]),

      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, N, Rest, Group);
    [Leader|Rest] ->
      io:format("Slave : ~w~n",[Id]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

crash(Id) ->
  case rand:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ -> 
      ok
  end.
