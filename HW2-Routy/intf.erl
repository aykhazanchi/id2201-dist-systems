-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

%return an empty interfaces list
new()->
    [].

%add a new interface
add(Name,Ref,Pid,Intf)->
    [{Name,Ref,Pid}|Intf].

%remove an interface
remove(Name,Intf)->
    lists:keydelete(Name,1,Intf).

%find the pid of an interface
lookup(Name, Intf)->
    Interface = lists:keyfind(Name,1,Intf),
    if 
        Interface == false ->
            notfound;
        true->
            {_,_,Pid} = Interface,
            {ok,Pid}            
        end.

%find the reference according to the name
ref(Name,Intf)->
    Interface = lists:keyfind(Name,1,Intf),
    if 
        Interface == false ->
            notfound;
        true->
            {_,Ref,_} = Interface,
            {ok,Ref}            
        end.

%find the name according to the reference
name(Ref,Intf)->
    Interface = lists:keyfind(Ref,2,Intf),
    if 
        Interface == false ->
            notfound;
        true->
            {Name,_,_} = Interface,
            {ok,Name}            
        end.

%return a list with all names
list(Intf)->
    lists:map(fun({Name,_,_})->Name end, Intf).

%send message to all interface progress
broadcast(Message,Intf)->
    lists:foreach(fun({_,_,Pid})->Pid!Message end, Intf).
  