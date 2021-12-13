-module(time).

-export([zero/0, inc/2, merge/2, leq/2, safe/2, clock/1, update/3]).

zero()->
  0.

inc(_, T) ->
  T+1.

merge(Ti, Tj) ->
  erlang:max(Ti, Tj).

leq(Ti, Tj) ->
  if
    Ti =< Tj ->
        true;
    Ti >= Tj ->
        false
  end.

%clock(Nodes) : return a clock that can keep track of the nodes
clock(Nodes) ->
    lists:map(fun(Node) -> {Node, 0} end, Nodes). % Initialize each Node to zero for start

%• update(Node, Time, Clock) : return a clock that has been updated
%given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, Time}).

%• safe(Time, Clock) : is it safe to log an event that happened at a given time, true or false
 safe(Time, Clock) ->
    % sort UpdatedClock first so least value is at the start
    UpdatedClock = lists:keysort(2, Clock), % sort Clock (list of nodes) on Time (position 2)
    [{_, Counter} | _] = UpdatedClock, % pull out Time/Counter from UpdatedClock 
    leq(Time, Counter). % and check against Time coming from loggy using leq