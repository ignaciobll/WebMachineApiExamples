-module(auth_bank).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([start/0, new/2, login/2]).

init(_) -> {ok, maps:new()}.

handle_call(Msg, _From, State) ->
  case Msg of
    {new, User, Pass} ->
      case lists:member(User, maps:keys(State)) of
        true ->
          {reply, {error, "user already exists"}, State};
        false ->
          {reply, ok, State#{ User => Pass }}
      end;
    {login, User, Pass} ->
      case maps:find(User, State) of
        {ok, Pass} ->
          {reply, ok, State};
        {ok, _} ->
          {reply, {error, "wrong password"}, State};
        error ->
          {reply, {error, "user not found"}, State}
      end
  end.

terminate(_,_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  {ok, Pid} = gen_server:start(?MODULE,[],[]),
  register(?MODULE, Pid),
  ?MODULE.

new(User, Password) ->
  gen_server:call(?MODULE, {new, User, Password}).

login(User, Pass) ->
  gen_server:call(?MODULE, {login, list_to_binary(User), list_to_binary(Pass)}).
