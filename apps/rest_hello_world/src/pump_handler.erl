%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(pump_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([send_update/2,get_update/2]).
-export([allowed_methods/2]).
-record(user, {node_token,sessionid,access_token,id,mng,sup,username}).

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

%From pump
get_update(Req,State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req, #{length => 1000000, period => 5000}),
    PumpIdBin = cowboy_req:binding(id, Req2),
    PumpId = erlang:binary_to_atom(PumpIdBin,utf8),

    NodeToken = binary_to_list(cowboy_req:binding(token, Req2)),
    {ok,User} = plantsys_usrmng:get_token(NodeToken),
    UserId = User#user.id,

    case plantsys_usrmng:get_pump(UserId,PumpId) of
        {error,_} ->  %%If no node exists
            plantsys_usrmng:add_pump(UserId,PumpId),
            io:format("PumpId: ~p~nStatus: ~p~n",[PumpId,Data]);
        _ -> undefined
    end,
    {true,Req2,State}.

%To pump
send_update(Req,State) ->
    PumpIdBin = cowboy_req:binding(id, Req),
    NodeToken = binary_to_list(cowboy_req:binding(token, Req)),
    {ok,User} = plantsys_usrmng:get_token(NodeToken),
    UserId = User#user.id,

    PumpId = erlang:binary_to_atom(PumpIdBin,utf8),
    case plantsys_usrmng:get_pump(UserId,PumpId) of
        {error,E} ->  %%If no node exists
            io:format("Error: ~p~n",[E]),
            plantsys_usrmng:add_pump(UserId,PumpId);
        _ -> undefined
    end,
    {ok,Status} = plantsys_usrmng:get_pumpdata(UserId,PumpId),
    Timer = maps:get(timer,Status),
    TStatus = maps:get(status,Status),


    Next = maps:get(next,Timer),
    {Me,S,_Mi} = erlang:timestamp(),

    Now = Me*1000000+S,
    Left = Next-Now, 

    TimeRun = maps:get(tr,Timer) div 1000,

    JsonStatus = jiffy:encode(#{status => TStatus, run => TimeRun, left => Left}),
    {JsonStatus, Req, State}.





































