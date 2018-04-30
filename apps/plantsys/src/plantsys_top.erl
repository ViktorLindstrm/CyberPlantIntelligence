%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-31 00:21:18.345253
%%%-------------------------------------------------------------------
-module(plantsys_top).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    %start_child().

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%start_child() ->
  %supervisor:start_child(?SERVER, []).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    UsrMngr = {'plantsys_usrmng', {'plantsys_usrmng', start_link, []},
              Restart, Shutdown, Type, ['plantsys_usrmng']},

    PlantSup = {'plantsys_usrsup', {'plantsys_usrsup', start_link, []},
              Restart, Shutdown, supervisor, ['plantsys_usrsup']},

    {ok, {SupFlags, [PlantSup,UsrMngr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
