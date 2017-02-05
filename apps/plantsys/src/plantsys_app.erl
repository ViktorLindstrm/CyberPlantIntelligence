%%%-------------------------------------------------------------------
%% @doc plantsys public API
%% @end
%%%-------------------------------------------------------------------

-module(plantsys_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    plantsys_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
