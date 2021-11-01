%%
%% Author:  Ayushman Khazanchi
%% Date:    15/9/2021
%% Class:   ID2201, HW1
%% Module:  rudy_multi, a web server with multiple threads to process requests
%% 

-module(rudy_multi).

%% Import modules we need
-import(http, [parse_request/1]).

%% Expose the init function that runs rudy_multi
-export([init/2, start/2, stop/0, controller/0]).

%% Internal functions

% Start server with multiple handlers in place
start(Port, N) ->
    register(rudy_multi, spawn(fun() -> init(Port, N) end)).

% Pass stop into the process via 'controller' to stop the server
stop() ->
    rudy_multi ! stop.

controller() ->
    receive
        stop ->
            ok
        end.

%% Initialize server with N number of handlers
init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handlers(Listen, N),
            controller();
        {error, Error} ->
            io:format("rudy_multi: error: ~w~n", [Error])
    end.

%% For each call use the same handlers function recursively 
handlers(Listen, N) ->
    case N of
        0 ->
            ok;
        N ->
            spawn(fun() -> handler(Listen) end),
            handlers(Listen, N-1)
        end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            io:format("rudy_multi: error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response),
            gen_tcp:close(Client);
        {error, Error} ->
            io:format("rudy_multi: error: ~w~n", [Error])
    end.

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
	http:ok("<html><head><title>rudy_multi</title></head><body>This is a multi-threaded test.<br/>" ++ URI ++ "</body></html>").
