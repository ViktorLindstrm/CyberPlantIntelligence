%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-30 23:36:40.380701
%%%-------------------------------------------------------------------
-module(plantsys_leds).
-behaviour(gen_server).

%% API
-export([start_link/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes=[],pumps=[],color=#{r=>0,g=>0,b=>0},alarm=false,id,name,timer=undefined}).

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
    {ok, #state{id=Id,name=atom_to_list(Id)}}.

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

handle_call({set_timer,{SH,SM},{EH,EM}}, _From, State) ->
    {_Date,{H,M,_}} = calendar:local_time(),
    Mins = case {H<SH,M<SM} of 
             {true,_} -> 
               (SH-H)*60+SM-M ;
             {false,_} -> 
               case H==SH of 
                 true -> 
                   SM-M;
                 false -> 
                   (24-(SH-H))*60+SM-M
               end
           end,
    Millis = Mins*60*1000,
    Timer = timer:apply_after(Millis,gen_server,call,[self(),{start_timer,{EH-SH,EM-SM}}]),
    NewState = State#state{timer=Timer},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({start_timer,{H,M}}, _From, #state{color=Color} = State) ->
    OnAgain = H*24+M*60*1000,
    timer:apply_after(OnAgain,gen_server,call,[self(),{set_color,{maps:get(r,Color),maps:get(g,Color),maps:get(b,Color)}}]),
    spawn(gen_server,call,[self(),{set_color,{0,0,0}}]), 
    Millis = 24*60*60*1000,

    Timer = timer:apply_after(Millis,gen_server,call,[self(),{start_timer,{H,M}}]),
    NewState = State#state{timer=Timer},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({stop_timer}, _From, #state{timer=Timer} = State) ->
  Reply = case Timer of 
            undefined -> 
              {ok,no_timer_set};
            _ -> 
              timer:cancel(Timer)
          end,
  {reply, Reply, State};

handle_call({get_color}, _From, #state{color=Color} = State) ->
    Reply = {ok,Color},
    {reply, Reply, State};

handle_call({set_color,{R,G,B}}, _From, State) ->
    NewState= State#state{color=#{r=>R,g=>G,b=>B}},
    Reply = ok,
    {reply, Reply, NewState};


handle_call({add_node,{_NodeId,_NodePid} = NodeIdPid}, _From, #state{nodes=Nodes} = State) ->
    NewNodes= [NodeIdPid|Nodes],
    NewState = State#state{nodes=NewNodes},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({remove_node,NodeId}, _From, #state{nodes=Nodes} = State) ->
    NewNodes = lists:keydelete(NodeId,1,Nodes),
    NewState = State#state{nodes=NewNodes},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({get_alarm}, _From, #state{alarm=Alarm} = State) ->
    Reply = {ok,Alarm},
    {reply, Reply, State};

handle_call({set_alarm,_Node}, _From, State) ->
  io:format("Alarm set"),
    NewState = State#state{alarm=true}, 
    Reply = ok,
    {reply, Reply, NewState};

handle_call({reset_alarm,_Node}, _From, State) ->
    io:format("Alarm REset"),
    NewState = State#state{alarm=false}, 
    Reply = ok,
    {reply, Reply, NewState}.
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
handle_cast(_Msg, State) ->
    {noreply, State}.

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

remove_node(_,[],R) -> R;
remove_node(NodeId,[NodeId|T],R) -> 
  remove_node(NodeId,T,R);

remove_node(NodeId,[Hd|T],R) -> 
  remove_node(NodeId,T,[Hd|R]).



