-module(counter).
-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([new/0, new/1, get/1, set/2, inc/1, dec/1, delete/1]).

init(_) -> {ok, 0}.

handle_call(Msg, _From, State) ->
    case Msg of
	{get} ->
	    {reply, State, State};
	{inc} ->
	    {reply, ok, State + 1};
	{dec} ->
	    {reply, ok, State - 1};
	{set, Value} ->
	    {reply, ok, Value};
	{terminate} ->
	    terminate(normal, State)
    end.

terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new() -> 
    gen_server:start(?MODULE, [], []).
    
new(InitialValue) -> 
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    set(Pid, InitialValue),
    Pid.

get(Counter) -> gen_server:call(Counter, {get}).

inc(Counter) -> gen_server:call(Counter, {inc}).

dec(Counter) -> gen_server:call(Counter, {dec}).

set(Counter, Value) -> gen_server:call(Counter, {set, Value}).

delete(Counter) -> gen_server:call(Counter, {terminate}).
