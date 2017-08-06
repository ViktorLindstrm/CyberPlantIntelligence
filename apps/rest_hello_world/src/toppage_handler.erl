%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([add_datapoint/2]).
-export([allowed_methods/2]).

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
  io:format("Input datapoint: ~p~n",[Data]),
  NodeIdBin = cowboy_req:binding(id, Req2),
  NodeId = erlang:binary_to_atom(NodeIdBin,utf8),
  case plantsys_mng:find_node(NodeId) of 
    {error,_} ->  %%If no node exists
      plantsys_mng:add_node(NodeId);
      _ -> undefined
  end,
  {M,S,Mi} = erlang:timestamp(),
  Time = (M * 1000000 + S + 3600)*1000 ,
  io:format("Time: ~p~n~n",[Time]),
  InData = jiffy:decode(Data,[return_maps]),
  JsonData = maps:put(<<"timestamp">>,integer_to_binary(Time),InData),
  io:format("In: ~p~n~n",[JsonData]),
  plantsys_mng:new_datapoint(NodeId,JsonData),
  update_nodes(),
  {true,Req2,State}.

update_nodes() ->
      case whereis(websocket) of 
        undefined -> undefined;
        Pid -> Pid ! {new_node}
      end.

