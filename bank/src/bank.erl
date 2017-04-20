-module(bank).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([start/0, new/0, withdraw/2, deposit/2, transfer/3, balance/1, exists/1]).

init(_) -> {ok, maps:new()}.

handle_call(Msg, _From, State) ->
    case Msg of
        {new_account} ->
            case maps:keys(State) of
                [] ->
                    {reply, 0, #{ accounts => [0], 0 => 0}};
                _ ->
                    #{ accounts := [Last|_] = Accounts } = State,
                    {reply, Last+1, State#{ accounts := [Last+1|Accounts], Last+1 => 0 }}
            end;
        {withdraw, AccountNumber, Quantity} ->
            case maps:find(AccountNumber, State) of
                error ->
                    {reply, {error, "account doesn't exists"}, State};
                {ok, Balance} when Balance < Quantity ->
                    {reply, {error, "not enough money"}, State};
                {ok, Balance} ->
                    {reply, {ok, Balance-Quantity}, State#{ AccountNumber := Balance-Quantity }}
            end;
        {deposit, AccountNumber, Quantity} ->
            case maps:find(AccountNumber, State) of
                {ok, Balance} ->
                    #{ AccountNumber := Balance } = State,
                    {reply, {ok, Balance+Quantity}, State#{ AccountNumber := Balance+Quantity }};
                error ->
                    {reply, {error, "account doesn't exists"}, State}
            end;
        {transfer, FromAccount, ToAccount, Quantity} ->
            case {maps:find(FromAccount, State), maps:find(ToAccount, State)} of
                {error, _} ->
                    {reply, {error, "from account doesn't exists"}, State};
                {_, error} ->
                    {reply, {error, "to account doesn't exists"}, State};
                {{ok, FromBalance}, {ok, _}} when FromBalance < Quantity ->
                    {reply, {error, "not enough money"}, State};
                {{ok, FromBalance}, {ok, ToBalance}} ->
                    {reply, {ok, FromBalance-Quantity},
                     State#{ FromAccount := FromBalance-Quantity, ToAccount := ToBalance+Quantity }}
            end;
        {balance, AccountNumber} ->
            case maps:find(AccountNumber, State) of
                error ->
                    {reply, {error, "account doesn't exists"}, State};
                {ok, Balance} ->
                    {reply, Balance, State}
            end;
	{keys} ->
	    {reply, maps:keys(State), State}
    end.

terminate(_,_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = gen_server:start(?MODULE,[],[]),
    register(?MODULE, Pid),
    ?MODULE.

new() ->
    gen_server:call(?MODULE, {new_account}).

withdraw(AccountNumber, Quantity) ->
    gen_server:call(?MODULE, {withdraw, AccountNumber, Quantity}).

deposit(AccountNumber, Quantity) ->
    gen_server:call(?MODULE, {deposit, AccountNumber, Quantity}).

transfer(FromAccount, ToAccount, Quantity) ->
    gen_server:call(?MODULE, {transfer, FromAccount, ToAccount, Quantity}).

balance(AccountNumber) ->
    gen_server:call(?MODULE, {balance, AccountNumber}).

exists(AccountNumber) ->
    lists:member(AccountNumber, gen_server:call(?MODULE, {keys})).
