-module(bank_config).

-export([
         dispatch/0,
         web_config/0
        ]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
		   {[account, op, quantity, to_account], bank_resource, []},
                   {[account, op, quantity], bank_resource, []},
		   {[account, op], bank_resource, []},
		   {[op], bank_resource, []}
                  ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
     {ip, Ip},
     {port, Port},
     {log_dir, "priv/log"},
     {dispatch, dispatch()}
    ].
