%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-31 00:21:18.345253
%%%-------------------------------------------------------------------
-module(plantsys_mngsup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
start_link(Id) ->
    supervisor:start_link(?MODULE, [Id]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

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
init([Id]) ->
    io:format("id:  ~p~n",[Id]),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    %MngPid = plantsys_usrmng:get_user(Id),
    %MngPid = test,

    %AChild = {'plantsys_mng', {'plantsys_mng', start_link, [Id]},
              %Restart, Shutdown, Type, ['plantsys_mng']},

    BChild = {'plantsys_nodesup', {'plantsys_nodesup', start_link, [Id]},
              Restart, Shutdown, supervisor, ['plantsys_nodesup']},

    Leds = 'plantsys_ledssup',
    LedsSup = {Leds, {Leds, start_link, [Id]},
              Restart, Shutdown, supervisor, [Leds]},

    %UsrMngr = {'plantsys_usrmng', {'plantsys_usrmng', start_link, []},
              %Restart, Shutdown, Type, ['plantsys_usrmng']},

    PumpSup = {'plantsys_pumpsup', {'plantsys_pumpsup', start_link, [Id]},
              Restart, Shutdown, supervisor, ['plantsys_pumpsup']},

    {ok, {SupFlags, [BChild,PumpSup,LedsSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



