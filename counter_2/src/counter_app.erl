-module(counter_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    counter_sup:start_link().

stop(_State) ->
    ok.
