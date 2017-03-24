-module(bank).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([start/0, new/1, withdraw/3, deposit/3, transfer/4, balance/2, exists/2]).

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
                    {reply, "account doesn't exists", State};
                {ok, Balance} when Balance < Quantity ->
                    {reply, "not enough money", State};
                {ok, Balance} ->
                    {reply, ok, State#{ AccountNumber := Balance-Quantity }}
            end;
        {deposit, AccountNumber, Quantity} ->
            case maps:find(AccountNumber, State) of
                {ok, Balance} ->
                    #{ AccountNumber := Balance } = State,
                    {reply, ok, State#{ AccountNumber := Balance+Quantity }};
                error ->
                    {reply, "account doesn't exists", State}
            end;
        {transfer, FromAccount, ToAccount, Quantity} ->
            case {maps:find(FromAccount, State), maps:find(ToAccount, State)} of
                {error, _} ->
                    {reply, "from account doesn't exists", State};
                {_, error} ->
                    {reply, "to account doesn't exists", State};
                {{ok, FromBalance}, {ok, _}} when FromBalance < Quantity ->
                    {reply, "not enough money", State};
                {{ok, FromBalance}, {ok, ToBalance}} ->
                    {reply, ok,
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
    Pid.
%% case register(bank, Pid) of
%%     true  -> ok;
%%     false -> {error, "server already started"}
%% end.

new(Bank) ->
    gen_server:call(Bank,{new_account}).

withdraw(AccountNumber, Quantity, Bank) ->
    gen_server:call(Bank, {withdraw, AccountNumber, Quantity}).

deposit(AccountNumber, Quantity, Bank) ->
    gen_server:call(Bank, {deposit, AccountNumber, Quantity}).

transfer(FromAccount, ToAccount, Quantity, Bank) ->
    gen_server:call(Bank, {transfer, FromAccount, ToAccount, Quantity}).

balance(AccountNumber, Bank) ->
    gen_server:call(Bank, {balance, AccountNumber}).

exists(AccountNumber, Bank) ->
    lists:member(AccountNumber, gen_server:call(Bank, {keys})).
