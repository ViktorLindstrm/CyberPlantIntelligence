-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {sessionid=undefined}).
-record(user, {node_token,sessionid,access_token,id,mng,sup,username}).
init(Req, State) ->

    Cookies = cowboy_req:parse_cookies(Req),
    logger:debug("Cookies: ~p~n",[Cookies]),
    NewState = case lists:keyfind(<<"sessionid">>, 1,Cookies) of 
               {_,SessionID} -> 
                       #state{sessionid=SessionID};
               false -> 
                       State
           end,
	{cowboy_websocket, Req, NewState }.

websocket_init(State) ->
    erlang:register(websocket,self()),
	{ok, State}.

websocket_handle({text, <<"nodes">>}, State) ->
    SessionID  = State#state.sessionid,
    UserId = case plantsys_usrmng:validate_user(SessionID) of 
                 {ok,User} -> 
                     User#user.username;

                 {error,E} ->
                     logger:error("Error: ~p~n",[E]),
                     undefined
             end,


  {ok,Nodes} = plantsys_usrmng:get_nodes(UserId),
  NodesBin = jiffy:encode(Nodes),
	{reply, {text, << "Nodes: ", NodesBin/binary >>}, State};

websocket_handle({text, <<"pumps">>}, State) ->
    SessionID  = State#state.sessionid,
    UserId = case plantsys_usrmng:validate_user(SessionID) of 
                 {ok,User} -> 
                     User#user.username;

                 {error,E} ->
                     logger:error("Error: ~p~n",[E]),
                     undefined
    end,

  {ok,Pumps} = plantsys_usrmng:get_pumps(UserId),
  PumpsU = lists:map(fun(X) -> 
                         Timer = maps:get(timer,X),
                         X#{timer:=Timer#{ts:=""}}
                     end, Pumps),
  PumpsBin = jiffy:encode(PumpsU),

	{reply, {text, << "Pumps: ", PumpsBin/binary >>}, State};

websocket_handle({text, Msg}, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State};

websocket_info({new_node}, State) ->

    SessionID  = State#state.sessionid,
    UserId = case plantsys_usrmng:validate_user(SessionID) of 
        {ok,User} -> 
                     User#user.username;

        {error,E} ->
                     logger:error("Error: ~p~n",[E]),
            undefined
    end,

  {ok,Nodes} = plantsys_usrmng:get_nodes(UserId),
  NodesBin = jiffy:encode(Nodes),
	{reply, {text, << "Nodes: ", NodesBin/binary >>}, State};

websocket_info({new_pump}, State) ->
    SessionID  = State#state.sessionid,
    UserId = case plantsys_usrmng:validate_user(SessionID) of 
        {ok,User} -> 
                     User#user.username;

        {error,E} ->
                     logger:error("Error: ~p~n",[E]),
            undefined
    end,

  {ok,Pumps} = plantsys_usrmng:get_pumps(UserId),
  PumpsBin = jiffy:encode(Pumps),
	{reply, {text, << "Pumps: ", PumpsBin/binary >>}, State};

websocket_info(_Info, State) ->
	{ok, State}.
