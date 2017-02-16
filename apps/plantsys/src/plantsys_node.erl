%%%-------------------------------------------------------------------
%%% @author viktor
%%% @copyright (C) 2016, viktor
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-30 23:36:40.380701
%%%-------------------------------------------------------------------
-module(plantsys_node).

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {image=undefined,type=sensor,pump=undefined,users=[],limit = undefined, current = [], data=[],id,name}).

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

start_link(Id,Type) ->
  gen_server:start_link(?MODULE, [Id,Type], []).
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
init([Id,Type]) ->
  {ok, #state{type=Type,id=Id,name=atom_to_list(Id)}};
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
handle_call({terminate}, _From, State) ->
  {stop, normal,ok, State};

handle_call({set_pump,PumpId}, _From, State) ->
  NewState = State#state{pump=PumpId},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({get_pump}, _From, #state{pump=Pump} = State) ->
  Reply = {ok,Pump},
  {reply, Reply, State};

handle_call({add_data,DataPoint}, _From, #state{data=Data} = State) ->
  NewData = [DataPoint|Data],
  NewState = State#state{data=NewData,current=DataPoint},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({set_image,Image}, _From, State) ->
  NewState = State#state{image=Image},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({get_image}, _From, #state{image=Image} = State) ->
  Reply = {ok,Image},
  {reply, Reply, State};

handle_call({set_type,Type}, _From, State) ->
  NewState = State#state{type=Type},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({get_current}, _From, #state{id = Id,type=Type, name=Name,limit=Limit, current=Current} = State) ->
  Data = #{id => Id,name => Name, type => Type, current => Current,limit => Limit}, 
  Reply = {ok,Data},
  {reply, Reply, State};

handle_call({set_name,Name}, _From, State) ->
  NewState = State#state{name=Name},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({set_limit,Limit}, _From, State) ->
  NewState = State#state{limit=Limit},
  Reply = ok,
  {reply, Reply, NewState};

handle_call({get_data}, _From, #state{data=Data} = State) ->
  Reply = {ok,Data},
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
terminate(Reason, _State) ->
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




