%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(pump_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([send_update/2,get_update/2]).
-export([allowed_methods/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
    {<<"application/json">>, get_update}
   ],
		Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, send_update}
	], Req, State}.

get_update(Req,State) ->
  {ok, Data, Req2} = cowboy_req:read_body(Req, #{length => 1000000, period => 5000}),
  PumpIdBin = cowboy_req:binding(id, Req2),
  PumpId = erlang:binary_to_atom(PumpIdBin,utf8),
  case plantsys_usrmng:get_pump(viktor,PumpId) of 
    {error,_} ->  %%If no node exists
      plantsys_usrmng:add_pump(viktor,PumpId),
      io:format("PumpId: ~p~nStatus: ~p~n",[PumpId,Data]);
      _ -> undefined
  end,
  {true,Req2,State}.

send_update(Req,State) ->
  PumpIdBin = cowboy_req:binding(id, Req),
  PumpId = erlang:binary_to_atom(PumpIdBin,utf8),
  case plantsys_usrmng:get_pump(viktor,PumpId) of 
    {error,E} ->  %%If no node exists
      io:format("Error: ~p~n",[E]),
      plantsys_usrmng:add_pump(viktor,PumpId),
      io:format("PumpId: ~p~n",[PumpId]);
      _ -> undefined
  end,
  {ok,Status} = plantsys_usrmng:get_pumpdata(viktor,PumpId),
  
  JsonStatus = jiffy:encode(#{status => maps:get(status,Status)}),
	{JsonStatus, Req, State}.





%update_nodes() ->
      %case whereis(websocket) of 
        %undefined -> undefined;
        %Pid -> Pid ! {new_node}
      %end.

