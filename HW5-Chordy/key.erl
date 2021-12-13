-module(key).

-export([generate/0, between/3]).

generate() ->
    random:uniform(1000000000).

% if From < Key < To or Key == To -> true
% if From > To, ??
% if From == To, true
between(Key, From, To) ->
    if 
        From == To ->
            % if Pred and Succ are same, return true
            true;
        From < To ->
            % if Pred less than Succ then confirm that
            % our Key is greater than Pred AND less than or equal to Succ
            % This is our 'between' check
            (From < Key) and (Key =< To);
        From > To ->
            % if Pred is greater than Succ then confirm that
            % our Key is greater than Pred OR less than or equal to Succ
            (From < Key) or (Key =< To)
    end.
