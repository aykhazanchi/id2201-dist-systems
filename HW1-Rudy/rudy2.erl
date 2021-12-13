%% @author vasigarans
%% @doc @todo Add description to rudy.


-module(rudy2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/2]).
-import(http,[parse_request/1,ok/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
init(Port,N)->
	Opt=[list,{active,false},{reuseaddr,true}],
	case gen_tcp:listen(Port,Opt) of
		{ok,Listen}->
			%io:fwrite("started"),
			parallel_handlers(Listen,N,self()),
			%wait_for_threads(10);clea
			super(self());
		{error,Error}->
			io:fwrite("Rudy server initialization failed ~w~n",[Error]),
			error
			end.



super(_)->
	
	receive
		stop ->
			  ok
		end.



parallel_handlers(Listen,N,Pid)->
	if N==0->
		   ok;
	   true->
		   spawn_link(fun()->handler(Listen,Pid) end),
			parallel_handlers(Listen,N-1,Pid)
	end.

handler(Listen,Pid)->
	case gen_tcp:accept(Listen) of
		{ok,Client}->
			%io:fwrite("request has been received"),
			request(Client),
			gen_tcp:close(Client),
			handler(Listen,Pid);
			
			
			
		
		{error,Error}->
			io:fwrite("error inside handler ~w~n",[Error]),
			error
	end,
	io:format("~w",[Pid]),
	Pid!stop.



request(Client)->
	Recv=gen_tcp:recv(Client,0),
	case Recv of {ok,Str}->
					 Request=http:parse_request(Str),
					 Response=reply(Request),
					 gen_tcp:send(Client,Response);
		{error,Error}->
			io:fwrite("rudy: error: ~w~n",[Error])
	end.
	%gen_tcp:close(Client).


reply({{get,URI,_},_,_})->
	%timer:sleep(40),
	http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").
	
  





