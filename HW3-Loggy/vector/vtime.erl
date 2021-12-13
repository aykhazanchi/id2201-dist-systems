-module(vtime).

-export([zero/1, inc/2, merge/2, leq/2, safe/2, clock/1, update/3]).

zero(AllNodes) ->
  lists:map(fun(Node) -> {Node, 0} end, AllNodes).

inc(Name, T) ->
  case lists:keyfind(Name, 1, T) of
    {_,Counter} ->
      lists:keyreplace(Name, 1, T, {Name, Counter+1});
    false ->
      [{Name, 1} | T]
    end.

merge([], Time) ->
  Time;

merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, erlang:max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti} | merge(Rest, Time)]
  end.

leq([], _) ->
  true;

leq([{Name, Ti} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, Time);
        true ->
          false
      end;
        false ->
          false
  end.

clock(_) ->
  [].

%• update(Node, Time, Clock) : return a clock that has been updated
%given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
    {ListNode, Tj} = lists:keyfind(Node, 1, Time),
    case lists:keyfind(Node, 1, Clock) of
      {From, _} ->
        lists:keyreplace(From, 1, Clock, {ListNode, Tj});
    false ->
      [{ListNode, Tj} | Clock]
end.

%• safe(Time, Clock) : is it safe to log an event that happened at a given time, true or false
 safe(Time, Clock) ->
    leq(Time, Clock).