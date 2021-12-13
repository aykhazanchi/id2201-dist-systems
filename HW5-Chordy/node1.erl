-module(node1).

-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    % if we are alone, connect to ourselves
    {ok, {Id, self()}};

connect(Id, Peer) ->
    % if we're joining a ring, send a key to the other node
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor) ->
    receive
        % {key, Qref, Peer} : a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % {notify, New} : a new node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % {request, Peer} : a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % {status, Pred} : our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        state ->
            io:format(' Id : ~w~n Predecessor : ~w~n Successor : ~w~n', [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor);
        stop -> ok;
        _ ->
            io:format('Strange message received'),
            node(Id, Predecessor, Successor)
    end.

% Skey and Spid are keys and pids of current node's successor
% XKey and XPid are keys and pids of successor's predecessor
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % insert yourself here
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            % do nothing because Id is us
            Successor;
        {Skey, _} ->
            % insert yourself here since successor is currently pointing to itself
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                % If the key of the predecessor of our successor (Xkey) is between 
                % us and our successor we should of course adopt this node as our 
                % successor and run stabilization again.    
                true ->
                    Xpid ! {request, self()},
                    Pred;
                % If we should be in between the nodes we inform our successor of our existence.
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
                end
        end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

% notify is a way for a node to make a polite request of another node to add it as its predecessor
% the node that is being "notified" also has to do its own checks before it adds the new node as a pred
notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            % if the notified node's predecessor is set as nil then accept the new predecessor
            {Nkey, Npid};
        {Pkey, _} ->
            % if there is a predecessor then check where it stands wrt to our new notifier node
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % if notifier node is between our pred and us then add it as new pred
                    {Nkey, Npid};
                false ->
                    % if notified is not between us, return our Predecessor to it
                    Predecessor
            end
    end.

create_probe(Id,{_,Spid}) ->
    Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
    Duration = timer:now_diff(erlang:now(),T),
    io:format("~n Time =~w --- Nodes = ~w~n",[Duration, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
    Spid ! {probe,Ref,Nodes ++ [Id],T}.