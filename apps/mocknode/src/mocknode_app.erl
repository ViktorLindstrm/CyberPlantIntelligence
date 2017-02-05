%%%-------------------------------------------------------------------
%% @doc mocknode public API
%% @end
%%%-------------------------------------------------------------------

-module(mocknode_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mocknode_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
