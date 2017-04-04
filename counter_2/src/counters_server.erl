-module(counters_server).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([start/0, new/0, get/1, inc/1, dec/1, set/2, exists/1, get_ids/0]).

init(_) -> {ok, maps:new()}.

handle_call(Msg, _From, State) ->
    case Msg of
        {new} ->
            {ok, Pid} = counter:new(),
            CounterId =
                case maps:keys(State) of
                    [] -> 0;
                    _  -> lists:max(maps:keys(State)) + 1
                end,
            {reply, CounterId, State#{ CounterId => Pid }};
        {get, CounterId} ->
            case maps:find(CounterId, State) of
                error ->
                    {reply, {error, "counter doesn't exists"}, State};
                {ok, Pid} ->
                    {reply, counter:get(Pid), State}
            end;
        {inc, CounterId} ->
            case maps:find(CounterId, State) of
                error ->
                    {reply, {error, "counter doesn't exists"}, State};
                {ok, Pid} ->
                    counter:inc(Pid),
                    {reply, ok, State}
            end;
        {dec, CounterId} ->
            case maps:find(CounterId, State) of
                error ->
                    {reply, {error, "counter doesn't exists"}, State};
                {ok, Pid} ->
                    counter:dec(Pid),
                    {reply, ok, State}
            end;
        {set, CounterId, Value} ->
            case maps:find(CounterId, State) of
                error ->
                    {reply, {error, "counter doesn't exists"}, State};
                {ok, Pid} ->
                    counter:set(Pid, Value),
                    {reply, ok, State}
            end;
	{keys} ->
	    {reply, maps:keys(State), State};
        _ ->
            {reply, {error, "unknown message"}, State}
    end.

terminate(_, _) -> ok.

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    register(counters_server, Pid),
    {ok, Pid}.

new() -> gen_server:call(counters_server, {new}).

get(CounterId) -> gen_server:call(counters_server, {get, CounterId}).

inc(CounterId) -> gen_server:call(counters_server, {inc, CounterId}).

dec(CounterId) -> gen_server:call(counters_server, {dec, CounterId}).

set(CounterId, Value) -> gen_server:call(counters_server, {set, CounterId, Value}).

exists(CounterId) ->
    lists:member(CounterId, gen_server:call(counters_server, {keys})).

get_ids() -> 
    lists:map(fun(X) -> integer_to_list(X) end, gen_server:call(counters_server, {keys})).
