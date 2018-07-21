%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([add_datapoint/2]).
-export([allowed_methods/2]).
-record(user, {node_token,sessionid,access_token,id,mng,sup,username}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, add_datapoint}
     ],
     Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, hello_to_html},
      {<<"application/json">>, hello_to_json},
      {<<"text/plain">>, hello_to_text}
     ], Req, State}.

add_datapoint(Req,State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req, #{length => 1000000, period => 5000}),

    NodeToken = binary_to_list(cowboy_req:binding(token, Req2)),
    {ok,User} = plantsys_usrmng:get_token(NodeToken),
    UserId = User#user.id,

    NodeIdBin = cowboy_req:binding(id, Req2),
    NodeId = erlang:binary_to_atom(NodeIdBin,utf8),
    case plantsys_usrmng:find_node(UserId,NodeId) of
        {error,_} ->
            plantsys_usrmng:add_node(UserId,NodeId);
        _ -> undefined
    end,
    {M,S,_Mi} = erlang:timestamp(),
    Time = (M * 1000000 + S + 3600)*1000 ,
    InData = jiffy:decode(Data,[return_maps]),
    JsonData = maps:put(<<"timestamp">>,integer_to_binary(Time),InData),
    plantsys_usrmng:new_datapoint(UserId,NodeId,JsonData),
    update_nodes(),
    {true,Req2,State}.

update_nodes() ->
    case whereis(websocket) of
        undefined -> undefined;
        Pid -> Pid ! {new_node}
    end.

