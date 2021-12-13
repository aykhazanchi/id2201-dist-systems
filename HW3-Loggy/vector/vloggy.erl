-module(vloggy).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Queue = [],
    Clock = vtime:clock(Nodes),
    loop(Clock, Queue).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = vtime:update(From, Time, Clock),
            UpdatedQueue = [{From, Time, Msg} | Queue],
            % sort UpdatedQueue so smallest time is at the start
            SortedQueue = lists:keysort(2, UpdatedQueue), % sort UpdatedQueue on Time (position 2)
            UnsafeQueue = checkSafe(UpdatedClock, SortedQueue),
            loop(UpdatedClock, UnsafeQueue);
        stop ->
            ok
    end.
    
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

% recursively check 
checkSafe(Clock, Queue) ->
    % base condition of when queue is empty
    if Queue == [] ->
        [];
    true ->
        % Remove first tuple from Queue and check against it
        [{From, Time, Msg} | Rest] = Queue,
        case vtime:safe(Time, Clock) of
            % if Time in removed tuple is less than Clock, log it
            true -> 
                log(From, Time, Msg),
                checkSafe(Clock, Rest);
            false -> 
                % We need to return Queue eventually to remove all the successful entries
                Queue
        end
    end.
    

