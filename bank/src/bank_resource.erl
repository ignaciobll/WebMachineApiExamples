-module(bank_resource).
-export([init/1,
         uri_too_long/2,
         allowed_methods/2,
         malformed_request/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         post_is_create/2,
         create_path/2,
         allow_missing_post/2,
         process_post/2,
         to_json/2,
         from_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([Op]) ->
    {ok, #{ op => Op }}.
%% {{trace, "/tmp"}, #{ op => Op }}. %% Debug

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json} ], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

allowed_methods(ReqData, State) ->
    {['GET', 'POST', 'HEAD'], ReqData, State}.

uri_too_long(ReqData, State) ->
    {wrq:path_info(ReqData) < 3, ReqData, State#{ length => length(wrq:path_info(ReqData)) }}.

malformed_request(ReqData, State) ->
    Method = wrq:method(ReqData),
    Length = maps:get(length, State),
    case maps:get(op, State) of
        new when (Length == 0) and (Method == 'POST') ->
            {false, ReqData, State};
        get_info when (Length == 1) and (Method == 'GET') ->
            {false, ReqData, State#{ account => get_from_path(ReqData, account) }};
        deposit when (Length == 1) and (Method == 'POST') ->
            {false, ReqData, State#{ account => get_from_path(ReqData, account) }};
        withdraw when (Length == 1) and (Method == 'POST') ->
            {false, ReqData, State#{ account => get_from_path(ReqData, account) }};
        transfer when (Length == 1) and (Method == 'POST') ->
            {false, ReqData, State#{ account => get_from_path(ReqData, account) }};
        _ -> {true, ReqData, State}
    end.

to_json(ReqData, State) ->
    Account = maps:get(account, State),
    Json = {struct, [{account, Account}, {balance, bank:balance(Account)}]},
    {mochijson2:encode(Json), ReqData, State}.

resource_exists(ReqData, State) ->
    case maps:get(op, State) of
        new -> {true, ReqData, State};
        _   -> {bank:exists(maps:get(account, State)), ReqData, State}
    end.

%% POST

post_is_create(ReqData, State) ->
    {maps:get(op, State) == new, ReqData, State}.

create_path(ReqData, State) ->
    NewAccount = integer_to_list(bank:new()),
    Path = "/" ++ NewAccount ++ "/",
    {Path, ReqData, State#{ account => NewAccount }}.

from_json(ReqData, State) ->
    Json = {struct, [{account, maps:get(account, State)}, {balance, 0}]},
    {true, wrq:set_resp_body(mochijson2:encode(Json), ReqData), State}.

allow_missing_post(ReqData, State) ->
    {true, ReqData, State}.

process_post(ReqData, State) ->
    {struct, Values} = mochijson2:decode(wrq:req_body(ReqData)),
    Op = maps:get(op, State),
    Account = maps:get(account, State),
    Result =
        case Values of
            [{_, Quantity}] when Op == deposit ->
                bank:deposit(Account, Quantity);
            [{_, Quantity}] when Op == withdraw ->
                bank:withdraw(Account, Quantity);
            [{_, Quantity}, {to, ToAccount}] when Op == transfer ->
                bank:transfer(Account, ToAccount, Quantity)
        end,
    Json =
	case Result of
	    {ok, Balance} ->
		{struct, [{account, Account}, {balance, Balance}]};
	    {error, Reason} ->
		{struct, [{error, Reason}]}
	end,
    {true, wrq:set_resp_body(mochijson2:encode(Json), ReqData), State}.

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
