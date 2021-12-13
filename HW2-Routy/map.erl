-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).

% return empty map
new() ->
    [].

update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node, 1, Map),
    [{Node, Links} | NewMap].

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false -> [];
        {_, ListNodes} -> ListNodes
    end.

all_nodes(Map) ->
    NewMap = lists:flatmap(fun({Node, Links}) -> [Node|Links] end, Map),
    lists:usort(NewMap).