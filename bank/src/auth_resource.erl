-module(auth_resource).
-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         uri_too_long/2,
         malformed_request/2,
	 process_post/2
        ]).

init([Op]) ->
  {ok, #{ op => Op }}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json} ], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.

allowed_methods(ReqData, State) ->
  {['POST'], ReqData, State}.

uri_too_long(ReqData, State) ->
  {wrq:path_info(ReqData) < 1, ReqData, State#{ length => length(wrq:path_info(ReqData)) }}.

malformed_request(ReqData, State) ->
  Method = wrq:method(ReqData),
  Length = maps:get(length, State),
  case maps:get(op, State) of
    new when (Length == 0) and (Method == 'POST') ->
      {false, ReqData, State};
    _ -> {true, ReqData, State}
  end.

process_post(ReqData, State) ->
  {struct, Values} = mochijson2:decode(wrq:req_body(ReqData)),
  Op = maps:get(op, State),
  User = proplists:get_value(<<"user">>, Values),
  Pass = proplists:get_value(<<"pass">>, Values),
  case
    case Op of
      new when (User =/= undefined) and (Pass =/= undefined) ->
        auth_bank:new(User, Pass);
      _ ->
        {error, "wrong body"}
    end
  of
    ok ->
      Json = {struct, [{user, User}, {pass, Pass}]},
      {true, wrq:set_resp_body(mochijson:encode(Json), ReqData), State};
    {error, _} -> {{halt, 304}, ReqData, State}
  end.
