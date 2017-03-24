-module(db_counters).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3,terminate/2]).

-export([start/0,get/1]).

init(_) ->
    {ok,[]}.

handle_call(Msg,_From,State) ->
    case Msg of
	{get,IdCounter} ->
	    case lists:keymember(IdCounter, 1, State) of
		true -> 
		    {IdCounter, Value} = lists:keyfind(IdCounter, 1, State),
			{reply,Value,State};
		_ ->
		    PCounter = counter:new(0),
		    {reply, PCounter, [{IdCounter,PCounter} | State]}
	    end
    end.

terminate(_,_) ->
    ok.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start() ->
    {ok,Pid} = gen_server:start(?MODULE,[],[]),
    register(db, Pid).

get(IdCounter)->
    gen_server:call(db, {get, IdCounter}).
