%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-29 23:52:27.932933
%%%-------------------------------------------------------------------
-module(plantsys_mng).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([add_node/1,
         add_leds/1,
         get_client_secret/0,
         set_client_secret/1,
         get_ledsalarm/1,
         get_ledstimer/1,
         stop_ledstimer/1,
         get_leds/0,
         get_led/1,
         unset_leds/2,
         set_ledscolor/4,
         get_ledscolor/1,
         set_ledstimer/5,
         new_datapoint/2,
         remove_node/1,
         add_pump/1,
         stop_pumptimer/1,
         start_pump/1,
         stop_pump/1,
         start_pumptimer/3,
         get_pump/1,
         get_connected_pump/1,
         set_pump/2,
         set_leds/2,
         get_data/1,
         get_pumpdata/1,
         get_settings/1,
         set_limit/2,
         add_pumpnode/2,
         remove_pumpnode/2,
         %verify_node/1,
         set_name/2,
         get_nodes/0,
         get_pumps/0,
         set_image/2,
         get_image/1,
         find_node/1,
         get_node_token/0,
         generate_node_token/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {client_secret=undefined,node_token=undefined,id=undefined,nodesup,nodes=[],pumpsup,pumps=[],ledsup,leds=[]}).

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
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

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
init([Id]) ->
    plantsys_usrmng:reg_mng(Id,self()),
    NodeSup = list_to_atom(atom_to_list(Id)++"nodesup"),
    PumpSup = list_to_atom(atom_to_list(Id)++"pumpsup"),
    LedSup = list_to_atom(atom_to_list(Id)++"ledsup"),
    {ok, #state{id=Id,nodesup=NodeSup,pumpsup=PumpSup,ledsup=LedSup}}.

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

handle_call({get_node_token}, _From, #state{node_token=Token} = State) ->
    Reply = {ok,Token},
    {reply, Reply, State};

handle_call({generate_node_token}, _From, #state{id=Id} = State) ->
    Token = create_code(15),
    plantsys_usrmng:set_node_token(Id,Token),
    NewState = State#state{node_token = Token},
    Reply = {ok,Token},
    {reply, Reply, NewState};

handle_call({set_client_secret,ClientSecret}, _From, State) ->
    NewState = State#state{client_secret=ClientSecret},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({get_client_secret}, _From, #state{client_secret = ClientSecret} = State) ->
    Reply = {ok,ClientSecret},
    {reply, Reply, State};

handle_call({add_pump,PumpId}, _From, #state{pumpsup=PumpSup,pumps=Pumps} = State) ->
    {ok,PumpPid} = supervisor:start_child(PumpSup,[PumpId]),
    NewState = State#state{pumps=[{PumpId,PumpPid}|Pumps]},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({add_leds,LedsId}, _From, #state{leds=Leds,ledsup=LedSup} = State) ->
    {ok, LedsPid} = supervisor:start_child(LedSup,[LedsId]),
    NewState = State#state{leds=[{LedsId,LedsPid}|Leds]},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({add_node,NodeId}, _From, #state{nodes=Nodes,nodesup=NodeSup} = State) ->
    {ok, NodePid} = supervisor:start_child(NodeSup,[NodeId]),

    NewState = State#state{nodes=[{NodeId,NodePid}|Nodes]},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({start_pump,PumpId}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,Pid} ->
                    gen_server:call(Pid,{start_pump});
                false ->
                    {error,no_such_pump}
            end,
    {reply, Reply, State};

handle_call({stop_pump,PumpId}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,Pid} ->
                    gen_server:call(Pid,{stop_pump});
                false ->
                    {error,no_such_pump}
            end,
    {reply, Reply, State};

handle_call({set_ledscolor,{LedsId,{R,G,B}}}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{set_color,{R,G,B}});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};

handle_call({set_ledstimer,LedsId,{SH,SM},{EH,EM}}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{set_timer,{SH,SM},{EH,EM}});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};

handle_call({stop_ledstimer,LedsId}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{stop_timer});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};


handle_call({get_ledsalarm,LedsId}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_alarm});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};

handle_call({get_ledscolor,LedsId}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_color});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};

handle_call({get_ledstimer,LedsId}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_timer});
                false ->
                    {error,no_such_leds}
            end,
    {reply, Reply, State};

handle_call({start_pumptimer,{PumpId,WaitTime,RunTime}}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,Pid} ->
                    gen_server:call(Pid,{start_timer,{WaitTime,RunTime}});
                false ->
                    {error,no_such_pump}
            end,
    {reply, Reply, State};

handle_call({stop_pumptimer,PumpId}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,Pid} ->
                    gen_server:call(Pid,{stop_timer});
                false ->
                    {error,no_such_pump}
            end,
    {reply, Reply, State};

handle_call({add_data,{NodeId,Data}}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    logger:debug("Data: ~p~n",[Data]),
                    gen_server:call(Pid,{add_data,Data}),
                    ok;
                false ->
                    error
            end,
    {reply, Reply, State};

handle_call({remove_pump,PumpId}, _From, #state{nodes=Nodes,pumps=Pumps} = State) ->
    {NewState,Reply} = case lists:keyfind(PumpId,1,Pumps) of
                {_,PumpPid} ->
                    logger:debug("remove PumpId: ~p~n",[PumpId]),
                    {ok,PumpData} = gen_server:call(PumpPid,{get_current}),
                    [case lists:keyfind(NodeId,1,Nodes) of 
                         {_,NodePid} -> 
                             logger:debug("Remove_Pump: ~p",[NodeId]),
                             gen_server:call(NodePid,{set_pump,undefined});
                         _ -> 
                             logger:error("unable to find NodeId: ~p",[NodeId])
                     end
                     || NodeId <- maps:get(nodes,PumpData)],
                    gen_server:stop(PumpPid),
                    NewPumps = lists:keydelete(PumpId,1,Pumps),
                    {State#state{pumps=NewPumps},ok};
                false ->
                    {State,error}
            end,
    {reply, Reply, NewState};

handle_call({remove_pumpnode,{PumpId,NodeId}}, _From, #state{nodes=Nodes,pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,PumpPid} ->
                    case lists:keyfind(NodeId,1,Nodes) of
                        {NId,NodePid} ->
                            gen_server:call(PumpPid,{remove_node,NId}),
                            gen_server:call(NodePid,{set_pump,undefined}),
                            ok;
                        _ ->
                            {error,no_such_node}
                    end;
                false ->
                    error
            end,
    {reply, Reply, State};

handle_call({remove_node,NodeId}, _From, #state{nodes=Nodes,pumps=Pumps} = State) ->
    {NewNodes,Reply} = case lists:keyfind(NodeId,1,Nodes) of
                           {_,Pid} ->
                               lists:map(fun({_PumpId,PumpPid}) -> gen_server:call(PumpPid,{remove_node,NodeId}) end,Pumps),
                               Rep = gen_server:call(Pid,{terminate}),
                               N = lists:keydelete(NodeId,1,Nodes),
                               {N,Rep};
                           false ->
                               {Nodes,{error,no_such_node}}
                       end,
    NewState = State#state{nodes=NewNodes},
    {reply, Reply, NewState};

handle_call({get_data,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_data});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({get_image,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_image});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({set_image,{NodeId,Image}}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    gen_server:call(Pid,{set_image,Image});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({set_pump,{NodeId,PumpId}}, _From, #state{pumps=Pumps,nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,NodePid} ->
                    case lists:keyfind(PumpId,1,Pumps) of
                        {_,PumpPid} ->
                            gen_server:call(NodePid,{set_pump,PumpId}),
                            gen_server:call(PumpPid,{add_node,{NodeId,NodePid}});
                        false ->
                            {error_no_such_pump}
                    end;
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};


handle_call({set_leds,{NodeId,LedsId}}, _From, #state{leds=Leds,nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,NodePid} ->
                    case lists:keyfind(LedsId,1,Leds) of
                        {_,LedsPid} ->
                            gen_server:call(NodePid,{set_leds,{LedsId,LedsPid}}),
                            gen_server:call(LedsPid,{add_node,{NodeId,NodePid}});
                        false ->
                            {error_no_such_leds}
                    end;
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({unset_leds,{NodeId,LedsId}}, _From, #state{leds=Leds,nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,NodePid} ->
                    case lists:keyfind(LedsId,1,Leds) of
                        {_,LedsPid} ->
                            gen_server:call(NodePid,{unset_leds}),
                            gen_server:call(LedsPid,{remove_node,NodeId});
                        false ->
                            {error_no_such_leds}
                    end;
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};


handle_call({get_pump,PumpId}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                false -> {error,not_found};
                _     -> {ok,found}
            end,
    {reply, Reply, State};

handle_call({get_led,LedsId}, _From, #state{leds=Leds} = State) ->
    Reply = case lists:keyfind(LedsId,1,Leds) of
                false -> {error,not_found};
                {_,LedsPid} -> gen_server:call(LedsPid,{get_led})
            end,
    {reply, Reply, State};

handle_call({get_connected_pump,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_pump});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({get_connected_leds,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_leds});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({find_node,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                false -> {error,not_found};
                _     -> {ok,found}
            end,
    {reply, Reply, State};

handle_call({get_nodes}, _From, #state{nodes=Nodes} = State) ->
    NodeIds = lists:map(fun({_,Pid}) ->
                                {ok,NodeData} = gen_server:call(Pid,{get_current}),
                                NodeData
                        end, Nodes),
    Reply = {ok, NodeIds},
    {reply, Reply, State};

handle_call({get_pumps}, _From, #state{pumps=Pumps} = State) ->
    PumpsIds = lists:map(fun({_,Pid}) ->
                                 {ok,PumpData} = gen_server:call(Pid,{get_current}),
                                 PumpData
                         end, Pumps),
    Reply = {ok, PumpsIds},
    {reply, Reply, State};

handle_call({get_leds}, _From, #state{leds=Leds} = State) ->
    LedsIds = lists:map(fun({_,Pid}) ->
                                {ok,LedsData} = gen_server:call(Pid,{get_led}),
                                LedsData
                        end, Leds),
    Reply = {ok, LedsIds},
    {reply, Reply, State};

handle_call({get_pumpdata,PumpId}, _From, #state{pumps=Pumps} = State) ->
    Reply = case lists:keyfind(PumpId,1,Pumps) of
                {_,Pid} ->
                    gen_server:call(Pid,{get_current});
                false -> {error,no_such_node}
            end,
    {reply, Reply, State};

handle_call({get_node,NodeId}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                false   -> {error,not_found};
                {_,Pid} -> gen_server:call(Pid,{get_current})
            end,
    {reply, Reply, State};

handle_call({set_limit,{NodeId,Limit}}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->  gen_server:call(Pid,{set_limit,Limit});
                false -> error
            end,
    {reply, Reply, State};

handle_call({set_name,{NodeId,Name}}, _From, #state{nodes=Nodes} = State) ->
    Reply = case lists:keyfind(NodeId,1,Nodes) of
                {_,Pid} ->  gen_server:call(Pid,{set_name,Name});
                false -> error
            end,
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({reg_nodesup,Pid}, State) ->
    NewState = State#state{nodesup=Pid},
    {noreply, NewState};
handle_cast({reg_pumpsup,Pid}, State) ->
    NewState = State#state{pumpsup=Pid},
    {noreply, NewState};
handle_cast({reg_lesdup,Pid}, State) ->
    NewState = State#state{ledsup=Pid},
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


set_client_secret(ClientSecret) -> gen_server:call(?MODULE,{set_client_secret,ClientSecret}).
get_client_secret() -> gen_server:call(?MODULE,{get_client_secret}).
new_datapoint(NodeId,Data) -> gen_server:call(?MODULE,{add_data,{NodeId,Data}}).
get_data(NodeId) -> gen_server:call(?MODULE,{get_data,NodeId}).
find_node(NodeId) -> gen_server:call(?MODULE,{find_node,NodeId}).
get_nodes() -> gen_server:call(?MODULE,{get_nodes}).
set_limit(NodeId,Limit) -> gen_server:call(?MODULE,{set_limit,{NodeId,Limit}}).
get_settings(NodeId) -> gen_server:call(?MODULE,{get_node,NodeId}).
set_name(NodeId,Name) -> gen_server:call(?MODULE,{set_name,{NodeId,Name}}).
set_image(NodeId,Image) -> gen_server:call(?MODULE,{set_image,{NodeId,Image}}).
get_image(NodeId) -> gen_server:call(?MODULE,{get_image,NodeId}).
set_pump(NodeId,PumpId) -> gen_server:call(?MODULE,{set_pump,{NodeId,PumpId}}).
get_pump(NodeId) -> gen_server:call(?MODULE,{get_pump,NodeId}).
set_leds(NodeId,LedsId) -> gen_server:call(?MODULE,{set_leds,{NodeId,LedsId}}).
get_connected_pump(NodeId) -> gen_server:call(?MODULE,{get_connected_pump,NodeId}).
remove_node(NodeId) -> gen_server:call(?MODULE,{remove_node,NodeId}).
unset_leds(NodeId,LedsId) -> gen_server:call(?MODULE,{unset_leds,{NodeId,LedsId}}).
get_pumpdata(PumpId) -> gen_server:call(?MODULE,{get_pumpdata,PumpId}).
get_pumps() -> gen_server:call(?MODULE,{get_pumps}).
add_pumpnode(PumpId,Node) -> gen_server:call(?MODULE,{add_pumpnode,{PumpId,Node}}).
remove_pumpnode(PumpId,Node) -> gen_server:call(?MODULE,{remove_pumpnode,{PumpId,Node}}).
start_pump(PumpId) -> gen_server:call(?MODULE,{start_pump,PumpId}).
stop_pump(PumpId) -> gen_server:call(?MODULE,{stop_pump,PumpId}).
start_pumptimer(PumpId,WaitTime,RunTime) -> gen_server:call(?MODULE,{start_pumptimer,{PumpId,WaitTime,RunTime}}).
stop_pumptimer(PumpId) -> gen_server:call(?MODULE,{stop_pumptimer,PumpId}).

get_ledscolor(LedsId) -> gen_server:call(?MODULE,{get_ledscolor,LedsId}).
get_ledstimer(LedsId) -> gen_server:call(?MODULE,{get_ledstimer,LedsId}).
set_ledscolor(LedsId,R,G,B) -> gen_server:call(?MODULE,{set_ledscolor,{LedsId,{R,G,B}}}).
get_led(LedsId) -> gen_server:call(?MODULE,{get_led,LedsId}).
get_leds() -> gen_server:call(?MODULE,{get_leds}).
get_ledsalarm(LedsId)  -> gen_server:call(?MODULE,{get_ledsalarm,LedsId}).
set_ledstimer(LedsId,SH,SM,EH,EM) -> gen_server:call(?MODULE,{set_ledstimer,LedsId,{SH,SM},{EH,EM}}).
stop_ledstimer(LedsId) -> gen_server:call(?MODULE,{stop_ledstimer,LedsId}).

get_node_token() -> gen_server:call(?MODULE,{get_node_token}).
generate_node_token() -> gen_server:call(?MODULE,{generate_node_token}).

add_pump(PumpId) ->
    case whereis(websocket) of
        undefined -> undefined;
        Pid -> Pid ! {new_pump}
    end,
    gen_server:call(?MODULE,{add_pump,PumpId}).

add_leds(LedsId) ->
    case whereis(websocket) of
        undefined -> undefined;
        Pid -> Pid ! {new_leds}
    end,
    gen_server:call(?MODULE,{add_leds,LedsId}).

add_node(NodeId) ->
    case whereis(websocket) of
        undefined -> undefined;
        Pid -> Pid ! {new_node}
    end,
    gen_server:call(?MODULE,{add_node,NodeId}).

create_code(Size) ->
    BinToken = crypto:strong_rand_bytes(Size),
    [case X of
         43 -> 45;
         47 -> 95;
         L -> L
     end || X<- base64:encode_to_string(BinToken)].
