%%
%% Author:  Ayushman Khazanchi
%% Date:    15/9/2021
%% Class:   ID2201, HW1
%% Module:  rudy, a simple web server with http parsing
%% 

-module(rudy).

%% Import modules we need
-import(http, [parse_request/1]).

%% Expose the init function that runs rudy
-export([init/1]).

%% Internal functions
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            ok;
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
	http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").
