-module(hist).

-export([new/1, update/3]).

new(Name)->
    [{Name,inf}].

update(Node, N, History)->
   case lists:keyfind(Node, 1, History) of
	{Name, Len} ->
	    if 
            N > Len -> 
                NewHist = lists:keydelete(Name,1,History),
                {new, [{Node, N}|NewHist]};
            true ->
                old
	    end;
	false ->
        NewHist2 = lists:keydelete(Node,1,History),
	    {new, [{Node, N}|NewHist2]}
    end.
