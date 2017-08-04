%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-30 23:36:40.380701
%%%-------------------------------------------------------------------
-module(plantsys_pump).
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

-record(state, {nodes=[],status=undefined,timer,id,name}).

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
    {ok, #state{id=Id,name=atom_to_list(Id),timer=#{ts=>off,tw=>0,tr=>0}}}.

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
handle_call({get_current}, _From, #state{id = Id, name=Name,nodes=Nodes, status=Status, timer=Timer} = State) ->
    Data = #{id => Id,name => Name, nodes => Nodes, status=>Status, timer=>Timer}, 
    Reply = {ok,Data},
    {reply, Reply, State};

handle_call({stop_timer}, _From, #state{timer=Timer} = State) ->
  %io:format("stop pump timer : ~p~n",[Timer]),
    {ok,cancel} = timer:cancel(maps:get(ts,Timer)),
    Reply = ok,
    NewState = State#state{timer=Timer#{ts:=off}},
    {reply, Reply, NewState};

handle_call({start_timer,{WaitTime,RunTime}}, _From, #state{timer=Timer} = State) ->
    io:format("Timer hit!~n"),
    {ok,T} = timer:apply_after(WaitTime, gen_server, call, [self(),{start_timer,{WaitTime,RunTime}}]),
    gen_server:cast(self(),{pump_timer,on,RunTime}),
    NewState = State#state{timer=Timer#{ts:=T,tw:=WaitTime,tr:=RunTime}},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({pump_timer}, _From, State) ->
  io:format("Pumptimer"),
    Reply = ok,
    {reply, Reply, State};

handle_call({add_node,{NodeId,_NodePid}}, _From, #state{nodes=Nodes} = State) ->
    NewNodes= [NodeId|Nodes],
    NewState = State#state{nodes=NewNodes},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({remove_node,NodeId}, _From, #state{nodes=Nodes} = State) ->
    NewNodes = remove_node(NodeId,Nodes,[]),
    NewState = State#state{nodes=NewNodes},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({start_pump}, _From, #state{status=Status,nodes=Nodes} = State) ->
  {NewStatus, Reply} = case Status of
                         on ->
                           {Status,{error,pumping}};
                         _ -> 
                           case length(Nodes) of 
                             0 -> {Status,{error,no_node_attached}};
                             _ -> {on,ok}
                           end
                       end,
  NewState = State#state{status=NewStatus},
  {reply, Reply, NewState};

handle_call({stop_pump}, _From, #state{status=Status } = State) ->
  {NewStatus, Reply} = case Status of
                         off->
                           {Status, {error,not_pumping}};
                         _ -> 
                           {off, ok}
                       end,
  NewState = State#state{status=NewStatus},
  {reply, Reply, NewState};

handle_call({set_name,Name}, _From, State) ->
    NewState = State#state{name=Name},
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
handle_cast({pump_timer,S,T}, #state{status=Status} = State) ->
  io:format("Cast pump start, Status: ~p ~n",[Status]),
  NewState = case S of
                on when (Status == off) or (Status == undefined) ->
                  io:format("Pumping~n"),
                  gen_server:call(self(),{start_pump}),
                  timer:apply_after(T, gen_server, cast, [self(),{pump_timer,off,0}]),
                  State#state{status=on};
                off when Status == on -> 
                  io:format("Turning pump off~n"),
                  gen_server:call(self(),{stop_pump}),
                  State#state{status=off};
                _ -> 
                  io:format("error: pump timer missmatch~n"),
                  State
              end,
    {noreply, NewState};

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



