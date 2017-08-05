-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	%erlang:start_timer(1000, self(), <<"Hello!">>),
  erlang:register(websocket,self()),
	{ok, State}.

websocket_handle({text, <<"nodes">>}, State) ->
  {ok,Nodes} = plantsys_mng:get_nodes(),
  NodesBin = jiffy:encode(Nodes),
	{reply, {text, << "Nodes: ", NodesBin/binary >>}, State};

websocket_handle({text, <<"pumps">>}, State) ->
  {ok,Pumps} = plantsys_mng:get_pumps(),
  io:format("Pumps: ~p~n",[Pumps]),
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
  {ok,Nodes} = plantsys_mng:get_nodes(),
  NodesBin = jiffy:encode(Nodes),
	{reply, {text, << "Nodes: ", NodesBin/binary >>}, State};

websocket_info({new_pump}, State) ->
  {ok,Pumps} = plantsys_mng:get_pumps(),
  PumpsBin = jiffy:encode(Pumps),
	{reply, {text, << "Pumps: ", PumpsBin/binary >>}, State};

websocket_info(_Info, State) ->
	{ok, State}.
