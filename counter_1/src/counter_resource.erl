-module(counter_resource).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json} ], ReqData, State}.
 
-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    PathInfo = wrq:path_info(ReqData),
    io:format("~p~n", [PathInfo]),
    {id, IdCounter} = lists:keyfind(id, 1, PathInfo),
    Value = case lists:keyfind(op, 1, PathInfo) of
		{op, "get"} -> 
		    PCounter = db_counters:get(IdCounter),
		    counter:get(PCounter);
		{op, "inc"} -> 
		    PCounter = db_counters:get(IdCounter),
		    counter:inc(PCounter),
		    counter:get(PCounter);
		{op, "dec"} -> 
		    PCounter = db_counters:get(IdCounter),
		    counter:dec(PCounter),
		    counter:get(PCounter);
		{op, "set"} -> 
		    PCounter = db_counters:get(IdCounter),
		    {oparg, SetArg} = lists:keyfind(oparg, 1, PathInfo),
		    case string:to_integer(SetArg) of
			{error, no_integer} -> no_integer;
			{Number, _} -> 
			    counter:set(PCounter, Number),
			    counter:get(PCounter)
		    end;
		{op, "delete"} -> 
		    PCounter = db_counters:get(IdCounter),
		    counter:delete(PCounter),
		    counter:get(PCounter);
		_ -> error
	    end,
    Content = {struct, [{IdCounter, Value}]},
    JContent = mochijson:encode(Content),
    {JContent, ReqData, State}.


-spec with_oparg(wrq:reqdata()) -> boolean().
with_oparg(ReqData) ->
    PathInfo = wrq:path_info(ReqData),
    case lists:keyfind(oparg, 1, PathInfo) of
	{oparg, _} -> true;
	_ -> false
    end.
