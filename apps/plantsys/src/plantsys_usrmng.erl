%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2017, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2017-01-21 13:23:59.006890
%%%-------------------------------------------------------------------
-module(plantsys_usrmng).

-behaviour(gen_server).
-export([
         add_node/2,
         add_leds/2,
         get_ledsalarm/2,
         get_ledstimer/2,
         stop_ledstimer/2,
         get_leds/1,
         get_led/2,
         unset_leds/3,
         set_ledscolor/5,
         get_ledscolor/2,
         set_ledstimer/6,
         new_datapoint/3,
         remove_node/2,
         add_pump/2,
         stop_pumptimer/2,
         start_pump/2,
         stop_pump/2,
         start_pumptimer/4,
         get_pump/2,
         get_connected_pump/2,
         set_pump/3,
         set_leds/3,
         get_data/2,
         get_pumpdata/2,
         get_settings/2,
         set_limit/3,
         add_pumpnode/3,
         remove_pumpnode/3,
         set_name/3,
         get_nodes/1,
         get_pumps/1,
         set_image/3,
         get_image/2,
         find_node/2
        ]).



-export([add_user/1,
        reg_mng/2,
        report_ledsup/2,
        get_user/1]).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {users=[]}).
-record(user, {id,mng,sup,username}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%-record(user, {id,sup,username, password}).
handle_call({add_user,UserId}, _From, #state{users=Users} = State) ->
    {_Status,NewState} = case plantsys_usrsup:start_child(UserId) of 
                             {ok, UserPid} ->  
                                 User = #user{id=UserId,sup=UserPid,username=UserId},
                                 {ok,State#state{users=[{UserId,User}|Users]}};
                             _ -> 
                                 {error,State}
                         end,
    Reply = ok,
    {reply, Reply, NewState};

handle_call({get_user,UserId}, _From, #state{users=Users} = State) ->
    Reply = case lists:keysearch(UserId,1,Users) of 
                {value,{UserId,User}} -> 
                    Sup = User#user.mng,
                    case Sup of
                        undefined -> 
                            {error,mng_not_registered};
                        _ -> 
                            {ok,Sup}
                    end;
                E ->
                    io:format("Error: ~p~n",[E]),
                    {error,no_such_user}
            end,
    {reply, Reply, State};

handle_call({report_ledsup,{UserId,Pid}}, _From, #state{users=Users} = State) ->
    Reply = case lists:keysearch(UserId,1,Users) of 
                {value,{UserId,User}} -> 
                    Sup = User#user.mng,
                    case Sup of
                        undefined -> 
                            {error,mng_not_registered};
                        _ -> 
                            gen_server:call(Sup,{reg_lesdup,Pid})
                    end;
                E ->
                    io:format("Error: ~p~n",[E]),
                    {error,no_such_user}
            end,
    {reply, Reply, State}.


%handle_call({reg_mng,{UserId,MngPid}}, _From, #state{users=Users} = State) ->
    %{Reply,NewState} = case lists:keysearch(UserId,1,Users) of 
                           %{UserId,User} -> 
                               %NewUser = User#user{mng=MngPid},
                               %NewUsers = lists:keyreplace(UserId,1,Users,NewUser),
                               %{ok,State#state{users=NewUsers}};
                           %_ ->
                               %{{error,no_such_user},State}
                       %end,
    %{reply, Reply, NewState}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%handle_cast(_Msg, State) ->
    %{noreply, State}.
handle_cast({reg_mng,{UserId,MngPid}}, #state{users=Users} = State) ->
    io:format("Reg ~p,Pid: ~p~n",[UserId,MngPid]),
    NewState = case lists:keysearch(UserId,1,Users) of 
                           {value,{UserId,User}} -> 
                               NewUser = User#user{mng=MngPid},
                               NewUsers = lists:keyreplace(UserId,1,Users,{UserId,NewUser}),
                               State#state{users=NewUsers};
                           false ->
                                State
                       end,
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_user(Id) -> gen_server:call(?MODULE,{add_user,Id}).
get_user(Id) -> gen_server:call(?MODULE,{get_user,Id}).
reg_mng(Id,MngPid) -> gen_server:cast(?MODULE,{reg_mng,{Id,MngPid}}).
report_ledsup(Id,SupPid) -> gen_server:call(?MODULE,{report_ledsup,{Id,SupPid}}).

add_pump(UserId,PumpId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{add_pump,PumpId}).
add_node(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{add_node,NodeId}).
add_leds(UserId,LedsId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{add_leds,LedsId}).



new_datapoint(UserId,NodeId,Data) -> {ok,Pid} = get_user(UserId),  gen_server:call(Pid,{add_data,{NodeId,Data}}).
get_data(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_data,NodeId}).
find_node(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{find_node,NodeId}).
get_nodes(UserId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_nodes}).
set_limit(UserId,NodeId,Limit) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{set_limit,{NodeId,Limit}}).
get_settings(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_node,NodeId}).
set_name(UserId,NodeId,Name) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{set_name,{NodeId,Name}}).
set_image(UserId,NodeId,Image) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{set_image,{NodeId,Image}}).
get_image(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_image,NodeId}).
set_pump(UserId,NodeId,PumpId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{set_pump,{NodeId,PumpId}}).
get_pump(UserId,NodeId) -> {ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_pump,NodeId}).
set_leds(UserId,NodeId,LedsId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{set_leds,{NodeId,LedsId}}).
get_connected_pump(UserId,NodeId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_connected_pump,NodeId}).
remove_node(UserId,NodeId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{remove_node,NodeId}).
unset_leds(UserId,NodeId,LedsId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{unset_leds,{NodeId,LedsId}}).
get_pumpdata(UserId,PumpId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_pumpdata,PumpId}).
get_pumps(UserId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{get_pumps}).
add_pumpnode(UserId,PumpId,Node) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{add_pumpnode,{PumpId,Node}}).
remove_pumpnode(UserId,PumpId,Node) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{remove_pumpnode,{PumpId,Node}}).
start_pump(UserId,PumpId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{start_pump,PumpId}).
stop_pump(UserId,PumpId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{stop_pump,PumpId}).
start_pumptimer(UserId,PumpId,WaitTime,RunTime) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{start_pumptimer,{PumpId,WaitTime,RunTime}}).
stop_pumptimer(UserId,PumpId) ->{ok,Pid} = get_user(UserId), gen_server:call(Pid,{stop_pumptimer,PumpId}).

get_ledscolor(UserId,LedsId) -> {ok,Pid} = get_user(UserId),gen_server:call(Pid,{get_ledscolor,LedsId}).
get_ledstimer(UserId,LedsId) ->  {ok,Pid} = get_user(UserId),gen_server:call(Pid,{get_ledstimer,LedsId}).
set_ledscolor(UserId,LedsId,R,G,B) ->   {ok,Pid} = get_user(UserId),gen_server:call(Pid,{set_ledscolor,{LedsId,{R,G,B}}}).
get_led(UserId,LedsId) ->    {ok,Pid} = get_user(UserId),gen_server:call(Pid,{get_led,LedsId}).
get_leds(UserId) -> {ok,Pid} = get_user(UserId),gen_server:call(Pid,{get_leds}).
get_ledsalarm(UserId,LedsId)  -> {ok,Pid} = get_user(UserId),gen_server:call(Pid,{get_ledsalarm,LedsId}).
set_ledstimer(UserId,LedsId,SH,SM,EH,EM) -> {ok,Pid} = get_user(UserId),gen_server:call(Pid,{set_ledstimer,LedsId,{SH,SM},{EH,EM}}).
stop_ledstimer(UserId,LedsId) -> {ok,Pid} = get_user(UserId),gen_server:call(Pid,{stop_ledstimer,LedsId}).

