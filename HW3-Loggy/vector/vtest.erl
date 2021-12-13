-module(vtest).

-export([run/2]).

run(Sleep, Jitter) ->
    AllNodes = [john, paul, ringo, george],
    Log = vloggy:start(AllNodes),
    A = vworker:start(john, Log, 13, Sleep, Jitter, AllNodes),
    B = vworker:start(paul, Log, 23, Sleep, Jitter, AllNodes),
    C = vworker:start(ringo, Log, 36, Sleep, Jitter, AllNodes),
    D = vworker:start(george, Log, 49, Sleep, Jitter, AllNodes),
    vworker:peers(A, [B, C, D]),
    vworker:peers(B, [A, C, D]),
    vworker:peers(C, [A, B, D]),
    vworker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vloggy:stop(Log),
    vworker:stop(A),
    vworker:stop(B),
    vworker:stop(C),
    vworker:stop(D).