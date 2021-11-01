-module(vworker).

-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter, AllNodes) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, AllNodes) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, AllNodes) ->
    rand:seed(exsss, {Seed, Seed, Seed}),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, vtime:zero(AllNodes));
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, VectorArray)->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            NewCounter = vtime:merge(Time, VectorArray),
            UpdatedCounter = vtime:inc(Name, NewCounter),
            Log ! {log, Name, UpdatedCounter, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, UpdatedCounter);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = vtime:inc(Name, VectorArray),
        Message = {hello, rand:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).