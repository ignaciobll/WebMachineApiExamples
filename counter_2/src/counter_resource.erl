-module(counter_resource).
-export([init/1,
         uri_too_long/2,
         malformed_request/2,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         process_post/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

%% This function is called after Dispatch
init([]) ->
    {ok, maps:new()}.
%% {{trace, "/tmp"}, maps:new()}. %% Debug

%% function that checks if the request is too long, (if uri_too_long
%% -> true, else false)
uri_too_long(ReqData, State) ->
    {wrq:path_info(ReqData) < 4, ReqData, State#{ length => length(wrq:path_info(ReqData)) }}.

%% function that defines the allowed methods to the API (GET, PUT,
%% POST, DELETE, HEAD...)
allowed_methods(ReqData, State) ->
    {['GET', 'POST', 'HEAD'], ReqData, State}.

%% function that checks if a function is malformed (if malformed ->
%% true, else false)
malformed_request(ReqData, State) ->
    Method = wrq:method(ReqData),
    {ok, Length} = maps:find(length, State),
    case lists:keyfind(op, 1, wrq:path_info(ReqData)) of
        {op, "new"} when (Length == 1) and (Method == 'POST') ->
            {false, ReqData, State#{ op => new }};
        {op, "ids"} when (Length == 1) and (Method == 'GET') ->
            {false, ReqData, State#{ op => ids }};
        {op, "get"} when (Length == 2) and (Method == 'GET') ->
            {false, ReqData, State#{ op => get, id => get_from_path(ReqData, id) }};
        {op, "inc"} when (Length == 2) and (Method == 'POST')->
            {false, ReqData, State#{ op => inc, id => get_from_path(ReqData, id) }};
        {op, "dec"} when (Length == 2) and (Method == 'POST')->
            {false, ReqData, State#{ op => dec, id => get_from_path(ReqData, id) }};
        {op, "set"} when (Length == 3) and (Method == 'POST')->
            {false, ReqData, State#{ op => set, id => get_from_path(ReqData, id),
                                     value => get_from_path(ReqData, value) }};
        _ ->
            {true, ReqData, State}
    end.

%% function that checks if the counter of a request exists
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

%% function that gives the content types that the API provides (html,
%% json...)
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json} ], ReqData, State}.

%% function that process a post request (here is where we produce the
%% side effect to our resource)
process_post(ReqData, State) ->
    Json =
        case maps:get(op, State) of
            new ->
                {struct, [{id, integer_to_list(counters_server:new())}, {value, 0}]};
            inc ->
                {struct, [{result, counters_server:inc(maps:get(id, State))}]};
            dec ->
                {struct, [{result, counters_server:dec(maps:get(id, State))}]};
            set ->
                {struct, [{result, counters_server:set(maps:get(id, State),maps:get(value, State))}]}
        end,
    {true, wrq:set_resp_body(mochijson:encode(Json), ReqData), State}.

%% function that creates the json response
to_json(ReqData, State) ->
    Json =
        case maps:get(op, State) of
            get ->
                {struct, [{id, maps:get(id, State)}, {value, counters_server:get(maps:get(id, State))}]};
            ids ->
                {struct, [{ids, {array, counters_server:get_ids()}}]}
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
