-module(node3).
-export([start/1, start/2, stop/0, init/2, node/5, notify/4, request/3, stabilize/4, monit/1]).

-define(Stabilize, 2000).
-define(Timeout, 5000).

start(Id) ->
	start(Id, nil).

start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).	

stop() ->
	exit(self()).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, nil, []). 

connect(Id, nil) ->
	{ok, {Id, nil, self()}};

connect(_, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			MonitorPid = monitor(Peer),
			{ok, {Skey, MonitorPid, Peer}} 
	after ?Timeout ->
		io:format("Timeout: no response from ~w~n", [Peer])
	end.

node(Id, Predecessor, Successor, Nx, Store) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Nx, Store);
		{notify, New} ->
			{Pred, St} = notify(New, Id, Predecessor, Store),	
			node(Id, Pred, Successor, Nx, St);
		{request, Peer} ->
			request(Peer, Predecessor, Successor),
			node(Id, Predecessor, Successor, Nx, Store);
		{status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
			node(Id, Predecessor, Succ, Nxt, Store);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Nx, Store);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Nx, Added);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Nx, Store);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Nx, Merged);
		{'DOWN', Ref, process, _, _} ->
			io:format("71 ref ~w Predecessor ~w Successor ~w Nx ~w~n", [Ref, Predecessor, Successor, Nx]),
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Nx),
			node(Id, Pred, Succ, Nxt, Store);
		probe ->
			create_probe(Id, Store, Successor),
			node(Id, Predecessor, Successor, Nx, Store);
		{probe, Id, Nodes, T} ->
			remove_probe(Id, Store, Nodes, T),
			node(Id, Predecessor, Successor, Nx, Store);
		{probe, RefKey, Nodes, T} ->
			forward_probe(RefKey, [Id|Nodes], Store, T, Successor),
			node(Id, Predecessor, Successor, Nx, Store);	
		stop ->
			stop()
	end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Added = storage:add(Key, Value, Store),
			Client ! {Qref, ok},
			Added;
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key , Pkey , Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			Spid ! {lookup, Key, Qref, Client}
	end.


notify({Nkey, _, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Nref = monitor(Npid),
			Keep = handover(Store, Id, Nkey, Npid),
			{{Nkey, Nref, Npid}, Keep};
		{Pkey, Pref, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					demonitor(Pref),
					Nref = monitor(Npid),
					Keep = handover(Store, Id, Nkey, Npid),
					{{Nkey, Nref, Npid}, Keep};
				false ->
					monitor(Npid),
					{Predecessor, Store}
			end
	end.

handover(Store, Id, Nkey, Npid) ->
	{Keep, Leave} = storage:split(Id, Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

request(Peer, Predecessor, {Skey, Sref, Spid}) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, {Skey, Sref, Spid}};
		{Pkey, Pref, Ppid} ->
			Peer ! {status, {Pkey, Pref, Ppid}, {Skey, Sref, Spid}}
	end.

stabilize(Pred, Nx, Id, Successor) -> %%monitor
	{Skey, Sref, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id, nil, self()}},
			{Successor, Nx};
		{Id, _, _} ->
			{Successor, Nx};
		{Skey, _, _} ->
			Spid ! {notify, {Id, nil, self()}},
			{Successor, Nx};
		{Xkey, _, Xpid}	 ->
			case key:between(Xkey, Id, Skey) of
				true ->
					Xref = monitor(Xpid),
					demonitor(Sref), 
					self() ! stabilize,
					{{Xkey, Xref, Xpid}, Successor};
				false ->
					Spid ! {notify, {Id, nil, self()}},
					{Successor, Nx}
	end
end.

stabilize({_, _,Spid}) ->
	%%io:format("168 Spid ~w~n", [Spid]),
	Spid ! {request, self()}.

monitor(Pid) ->
	erlang:monitor(process, Pid).

demonitor(nil) ->
	ok;

demonitor(MonitorPid) ->
	erlang:demonitor(MonitorPid, [flush]).

down(Ref, {_, Ref, _}, Successor, Nx) ->
	{nil, Successor, Nx};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
	self() ! stabilize,
 	Nref = monitor(Npid),
	{Predecessor, {Nkey, Nref, Npid}, nil}.

create_probe(Id, Store, {_, _, Spid}) ->
	Spid ! {probe, Id, [Id], erlang:now()},
	io:format("Create probe ~w! Store ~w~n", [Id, Store]).

remove_probe(Id, Store, Nodes, T) ->
	Time = timer:now_diff(erlang:now(), T),
	io:format("Received probe ~w in ~w ms Ring: ~w Store ~w~n", [Id, Time, Nodes, Store]).

forward_probe(RefKey, Nodes, Store, T, {_, _, Spid}) ->
	Spid ! {probe, RefKey, Nodes, T},
	io:format("Forward probe ~w! Store ~w~n", [RefKey, Store]).

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).