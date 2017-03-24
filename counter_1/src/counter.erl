-module(counter).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3,terminate/2]).

-export([new/1,get/1,set/2,delete/1, inc/1, dec/1]).

init(_) ->
    {ok,0}.

handle_call(Msg,_From,State) ->
    case Msg of
	{new,InitialValue} ->
	    {reply,ok,InitialValue};
	{get,_Counter} ->
	    {reply,State,State};
	{set,_Counter, Value} when is_integer(Value)->
	    {reply, ok, Value};
	{inc,_Counter} ->
	   {reply, ok, State + 1} ;
	{dec,_Counter} ->
	   {reply, ok, State - 1} ;		       
	{delete,_Counter} ->
	    {reply,ok,0}
    end.

terminate(_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(InitialValue) ->
    {ok,Pid} = gen_server:start(?MODULE,[],[]),
    set(Pid,InitialValue),
    Pid.

get(Counter) ->
    gen_server:call(Counter,{get, Counter}).

set(Counter,Value) ->
    gen_server:call(Counter,{set, Counter, Value}).

delete(Counter) ->
    gen_server:call(Counter,{delete,Counter}).

inc(Counter) ->
    gen_server:call(Counter, {inc, Counter}).

dec(Counter) ->
    gen_server:call(Counter, {dec, Counter}).

