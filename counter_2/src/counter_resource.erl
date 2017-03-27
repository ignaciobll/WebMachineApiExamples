-module(counter_resource).
-export([init/1,
         uri_too_long/2,
         malformed_request/2,
         resource_exists/2,
         %% valid_content_headers/2,
         %% allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    %% {ok, maps:new()}.
    {{trace, "/tmp"}, maps:new()}. %% Debug

uri_too_long(ReqData, State) ->
    {wrq:path_info(ReqData) < 4, ReqData, State#{ length => length(wrq:path_info(ReqData))}}.

malformed_request(ReqData, State) ->
    {ok, Length} = maps:find(length, State),
    case lists:keyfind(op, 1, wrq:path_info(ReqData)) of
        {op, "new"} when Length == 1 ->
            {false, ReqData, State#{ op => new }};
        {op, "get"} when Length == 2 ->
            {false, ReqData, State#{ op => get, id => get_from_path(ReqData, id) }};
        {op, "inc"} when Length == 2 ->
            {false, ReqData, State#{ op => inc, id => get_from_path(ReqData, id) }};
        {op, "dec"} when Length == 2 ->
            {false, ReqData, State#{ op => dec, id => get_from_path(ReqData, id) }};
        {op, "set"} when Length == 3 ->
            {false, ReqData, State#{ op => set, id => get_from_path(ReqData, id), 
				     value => get_from_path(ReqData, value) }};
        _ ->
            {true, ReqData, State}
    end.

resource_exists(ReqData, State) ->
    {lists:foldl(fun(X, Acc) ->
                         case maps:get(X, State) of
                             error -> 
				 false and Acc;
                             Value when X == id ->
				 counters_server:exists(Value) and Acc;
			     _ -> true and Acc
			 end
                 end, true, maps:keys(State)), ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json} ], ReqData, State}.

to_json(ReqData, State) ->
    Json =
        case maps:get(op, State) of
            new ->
                {struct, [{id, counters_server:new()}, {value, "0"}]};
            get ->
                {struct, [{id, maps:get(id, State)}, {value, counters_server:get(maps:get(id, State))}]};
            inc ->
                {struct, [{result, counters_server:inc(maps:get(id, State))}]};
            dec ->
                {struct, [{result, counters_server:dec(maps:get(id, State))}]};
            set ->
                {struct, [{result, counters_server:set(maps:get(id, State), maps:get(value, State))}]}
        end,
    {mochijson:encode(Json), ReqData, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_from_path(ReqData, Key) ->
    case lists:keyfind(Key, 1, wrq:path_info(ReqData)) of
        {Key, Id} ->
            case string:to_integer(Id) of
                {error, _} -> error;
                {Value, _} -> Value
            end;
        _ -> error
    end.
