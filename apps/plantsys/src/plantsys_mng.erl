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
-export([start_link/0]).

-export([add_node/1,
         new_datapoint/2,
         add_pump/1,
         start_pump/1,
         stop_pump/1,
         get_data/1,
         get_pumpdata/1,
         get_settings/1,
         set_limit/2,
         add_pumpnode/2,
         remove_pumpnode/2,
         verify_node/1,
         set_name/2,
         get_nodes/0,
         get_pumps/0,
         set_image/2,
         get_image/1,
         find_node/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes=[],pumps=[]}).

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

handle_call({add_pump,PumpId}, _From, #state{pumps=Pumps} = State) ->
  {ok, PumpPid} = plantsys_pumpsup:start_child(PumpId),
  %gen_server:call(PumpPid,{set_type,pump}),
  NewState = State#state{pumps=[{PumpId,PumpPid}|Pumps]},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({add_node,NodeId}, _From, #state{nodes=Nodes} = State) ->
  {ok, NodePid} = plantsys_nodesup:start_child(NodeId),
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

handle_call({add_data,{NodeId,Data}}, _From, #state{nodes=Nodes} = State) ->
  Reply = case lists:keyfind(NodeId,1,Nodes) of 
            {_,Pid} -> 
              gen_server:call(Pid,{add_data,Data}),
              ok;
            false -> 
              error
          end,
  {reply, Reply, State};

handle_call({add_pumpnode,{PumpId,NodeId}}, _From, #state{nodes=Nodes,pumps=Pumps} = State) ->
  Reply = case lists:keyfind(PumpId,1,Pumps) of 
            {_,Pid} -> 
              case lists:keyfind(NodeId,1,Nodes) of 
                {NId,NPid} -> gen_server:call(Pid,{add_node,{NId,NPid}}),
                ok;
                _ -> {error,no_such_node}
              end;
            false -> 
              error
          end,
  {reply, Reply, State};


handle_call({remove_pumpnode,{PumpId,NodeId}}, _From, #state{nodes=Nodes,pumps=Pumps} = State) ->
  Reply = case lists:keyfind(PumpId,1,Pumps) of 
            {_,Pid} -> 
              case lists:keyfind(NodeId,1,Nodes) of 
                {NId,_} -> gen_server:call(Pid,{remove_node,NId}),
                ok;
                _ -> {error,no_such_node}
              end;
            false -> 
              error
          end,
  {reply, Reply, State};

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


add_node(NodeId) -> 
  case whereis(websocket) of 
    undefined -> undefined;
    Pid -> Pid ! {new_node}
  end,
  gen_server:call(?MODULE,{add_node,NodeId}).


new_datapoint(NodeId,Data) -> gen_server:call(?MODULE,{add_data,{NodeId,Data}}).
get_data(NodeId) -> gen_server:call(?MODULE,{get_data,NodeId}).
find_node(NodeId) -> gen_server:call(?MODULE,{find_node,NodeId}).
get_nodes() -> gen_server:call(?MODULE,{get_nodes}).
set_limit(NodeId,Limit) -> gen_server:call(?MODULE,{set_limit,{NodeId,Limit}}).
get_settings(NodeId) -> gen_server:call(?MODULE,{get_node,NodeId}).
set_name(NodeId,Name) -> gen_server:call(?MODULE,{set_name,{NodeId,Name}}).
verify_node(NodeId) -> unimplemented.%gen_server:call(?MODULE,{verify,NodeId}).
set_image(NodeId,Image) -> gen_server:call(?MODULE,{set_image,{NodeId,Image}}).
get_image(NodeId) -> gen_server:call(?MODULE,{get_image,NodeId}).


get_pumpdata(PumpId) -> gen_server:call(?MODULE,{get_pumpdata,PumpId}).
get_pumps() -> gen_server:call(?MODULE,{get_pumps}).
add_pumpnode(PumpId,Node) -> gen_server:call(?MODULE,{add_pumpnode,{PumpId,Node}}).
remove_pumpnode(PumpId,Node) -> gen_server:call(?MODULE,{remove_pumpnode,{PumpId,Node}}).
start_pump(PumpId) -> gen_server:call(?MODULE,{start_pump,PumpId}).
stop_pump(PumpId) -> gen_server:call(?MODULE,{stop_pump,PumpId}).

add_pump(PumpId) -> 
  case whereis(websocket) of 
    undefined -> undefined;
    Pid -> Pid ! {new_pump}
  end,
  gen_server:call(?MODULE,{add_pump,PumpId}).
