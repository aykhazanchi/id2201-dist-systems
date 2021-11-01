-module(storage).

-compile(export_all).

% Store is a list of tuples containing key value pairs

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

lookup(Key,Store)->
    lists:keyfind(Key,1,Store).

% updated store contains only request key value
% Rest are stored in a list called "Store"
split(From, To, Store) ->
	lists:partition(fun({Key,_}) -> key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
    lists:merge(Entries, Store).