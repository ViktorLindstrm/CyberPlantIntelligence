-module(leds_handler).
-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([send_update/2]).
-export([allowed_methods/2]).
-record(user, {node_token,sessionid,access_token,id,mng,sup,username}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      %{<<"application/json">>, get_update}
     ],
     Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, send_update}
     ], Req, State}.

send_update(Req,State) ->
    LedsIdBin = cowboy_req:binding(id, Req),
    LedsId = erlang:binary_to_atom(LedsIdBin,utf8),

    NodeToken = binary_to_list(cowboy_req:binding(token, Req)),
    User = plantsys_usrmng:get_token(NodeToken),
    UserId = User#user.id,

    case plantsys_usrmng:get_led(UserId,LedsId) of
        {error,not_found} ->  %%If no node exists
            %io:format("Error: ~p~n",[E]),
            plantsys_usrmng:add_leds(UserId,LedsId);
        %io:format("LedsId: ~p~n",[LedsId]);
        _ -> undefined
    end,
    {ok,Status} = case plantsys_usrmng:get_ledsalarm(UserId,LedsId) of
                      {ok,true} ->
                          {ok,#{r=>255,g=>0,b=>0}};
                      _ ->
                          plantsys_usrmng:get_ledscolor(UserId,LedsId)
                  end,
    %LedsRGB = maps:get(color,Status),
    JsonStatus = jiffy:encode(Status),
    io:format("Got hi from ~p~nanswering with: ~p~n",[LedsId,JsonStatus]),
    {JsonStatus, Req, State}.
