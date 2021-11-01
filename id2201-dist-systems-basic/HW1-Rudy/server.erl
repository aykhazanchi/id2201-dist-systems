%%
%% Author:  Ayushman Khazanchi
%% Date:    15/9/2021
%% Class:   ID2201, HW1
%% Module:  server control for starting/stopping rudy
%% 
 
-module(server).

%% Expose start and stop so they can be called from outside
-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun() -> rudy:init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die!").