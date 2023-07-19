%%%-------------------------------------------------------------------
%% @doc piper public API
%% @end
%%%-------------------------------------------------------------------

-module(piper_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    piper_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
