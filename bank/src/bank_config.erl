-module(bank_config).

-export([
         dispatch/0,
         web_config/0
        ]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
		   {["bank", "new"], bank_resource, [new]},
		   {["auth", "new"], auth_resource, [new]},
		   {["bank", account], bank_resource, [get_info]},
		   {["bank", account, "deposit"], bank_resource, [deposit]},
		   {["bank", account, "withdraw"], bank_resource, [withdraw]},
		   {["bank", account, "transfer"], bank_resource, [transfer]}
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
