-module(leds_handler).
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
  io:format("Data: ~p~n",[Data]),
  PumpIdBin = cowboy_req:binding(id, Req2),
  PumpId = erlang:binary_to_atom(PumpIdBin,utf8),
  case plantsys_mng:get_pump(PumpId) of 
    {error,_} ->  %%If no node exists
      plantsys_mng:add_pump(PumpId),
      io:format("PumpId: ~p~nStatus: ~p~n",[PumpId,Data]);
      _ -> undefined
  end,
  {true,Req2,State}.

send_update(Req,State) ->
  LedsIdBin = cowboy_req:binding(id, Req),
  LedsId = erlang:binary_to_atom(LedsIdBin,utf8),
  case plantsys_mng:get_led(LedsId) of 
    {error,not_found} ->  %%If no node exists
      %io:format("Error: ~p~n",[E]),
      plantsys_mng:add_leds(LedsId);
      %io:format("LedsId: ~p~n",[LedsId]);
      _ -> undefined
  end,
  {ok,Status} = case plantsys_mng:get_ledsalarm(LedsId) of
                  {ok,true} -> 
                    {ok,#{r=>255,g=>0,b=>0}};
                  _ ->
                    plantsys_mng:get_ledscolor(LedsId)
                end,
  %LedsRGB = maps:get(color,Status),
  JsonStatus = jiffy:encode(Status),
	{JsonStatus, Req, State}.

