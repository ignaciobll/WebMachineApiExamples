-module(bank_resource).
-export([init/1,
         uri_too_long/2,
         malformed_request/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, maps:new()}.

uri_too_long(ReqData, State) ->
    {wrq:path_info(ReqData) < 5, ReqData, State#{ length => length(wrq:path_info(ReqData)) }}.

malformed_request(ReqData, State) ->
    {ok, Length} = maps:find(length, State),
    case lists:keyfind(op, 1, wrq:path_info(ReqData)) of
        {op, "new"} when Length == 1 ->
            {false, ReqData, State#{ op => new }};
        {op, "balance"} when Length == 2 ->
            {false, ReqData, State#{ op => balance, account => get_from_path(ReqData, account) }};
        {op, "deposit"} when Length == 3 ->
            {false, ReqData,
             State#{ op => deposit , account => get_from_path(ReqData, account),
                     quantity => get_from_path(ReqData, quantity) }};
        {op, "withdraw"} when Length == 3 ->
            {false, ReqData,
             State#{ op => withdraw , account => get_from_path(ReqData, account),
                     quantity => get_from_path(ReqData, quantity) }};
        {op, "transfer"} when Length == 4 ->
            {false, ReqData,
             State#{ op => withdraw , account => get_from_path(ReqData, account),
                     quantity => get_from_path(ReqData, quantity),
                     to_account => get_from_path(ReqData, to_account) }};
        _ ->
            {true, ReqData, State}
    end.

resource_exists(ReqData, State) ->
    {lists:foldl(fun(X, Acc) ->
                         case maps:get(X, State) of
                             Value when X == account ->
                                 bank:exists(Value, bank) and Acc;
                             error ->
                                 false and Acc;
                             _ ->
                                 true and Acc
                         end
                 end, true, maps:keys(State)), ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Json =
        case maps:get(op, State) of
            new ->
                {struct, [{account, bank:new(bank)}, {balance, 0}]};
            balance ->
                {struct, [{account, maps:get(account, State)},
                          {balance, bank:balance(maps:get(account, State), bank)}]};
            deposit ->
                {struct, [{result, bank:deposit(maps:get(account, State),
                                                maps:get(quantity, State), bank)}]};
            withdraw ->
                {struct, [{result, bank:withdraw(maps:get(account, State),
                                                 maps:get(quantity, State), bank)}]};
            transfer ->
                {struct, [{result, bank:transfer(maps:get(account, State), maps:get(to_account, State),
                                                 maps:get(quantity, State), bank)}]}
        end,
    {mochijson:encode(Json), ReqData, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_from_path(ReqData, Key) ->
    case lists:keyfind(Key, 1, wrq:path_info(ReqData)) of
        {Key, Id} ->
            case string:to_integer(Id) of
                {error, _} -> error;
                {Value, _} -> Value
            end;
        _ -> error
    end.
