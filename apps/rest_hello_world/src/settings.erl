-module(settings).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{node := Node,pump:=Pump} = cowboy_req:match_qs([{node, [], undefined},{pump, [], undefined}], Req0),
  Req = case Node of
          undefined -> 
            case Pump of 
              undefined -> 
                echo(Method,undefined,Req0);
              _ -> 
                pump(Method, Pump, Req0)
            end;
          _-> 
            sensor(Method, Node, Req0)
        end,

  {ok, Req, Opts}.

pump_action({<<"add">>,Node},Pump) -> 
  NodeId = binary_to_atom(Node,utf8),
  PumpId = binary_to_atom(Pump,utf8),
  %plantsys_mng:add_pumpnode(PumpId,NodeId);
  plantsys_mng:set_pump(NodeId,PumpId);

pump_action({<<"remove">>,Node},Pump) -> 
  NodeId = binary_to_atom(Node,utf8),
  PumpId = binary_to_atom(Pump,utf8),
  plantsys_mng:remove_pumpnode(PumpId,NodeId);

pump_action({<<"status">>,Current},Pump) -> 
  PumpId = binary_to_atom(Pump,utf8),
  case Current of
    <<"undefined">> -> 
      plantsys_mng:start_pump(PumpId);
    <<"on">> -> 
      plantsys_mng:stop_pump(PumpId);
    <<"off">> -> 
      plantsys_mng:start_pump(PumpId)
  end.

pump(<<"POST">>, Pump, Req0) ->
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  pump_action(hd(PostVals),Pump),
  
  cowboy_req:reply(302, #{
    <<"Location">> => <<"/settings?pump=",Pump/binary>>
   }, Req);

pump(<<"GET">>, Pump, Req0) ->
  PumpId = erlang:binary_to_atom(Pump,utf8),
  {ok,PumpData} = plantsys_mng:get_pumpdata(PumpId),
  Connected = maps:get(nodes,PumpData),
  Status = maps:get(status,PumpData),
  Title = "Pump Settings",
  Body = pump_body(Pump,Status,Connected), 
  Head = head(),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/html">>
   }, ["<html><head><title>", Title, "</title>",Head,"</head>",
       "<body>",Body,"</body></html>"], Req0).

pump_body(PumpId,Status,Connected) -> 
  {ok,Nodes} = plantsys_mng:get_nodes(),
  NodeIds= lists:map(fun(X) -> 
                         {maps:get(id,X),list_to_atom(maps:get(name,X))} 
                     end,lists:reverse(Nodes)),

  CIds = lists:map(fun(Id) -> lists:keyfind(Id,1,NodeIds) end,Connected),

  NConnected = NodeIds -- CIds,

  NButtons = lists:map(fun({Y,X}) -> 
                           Name = atom_to_list(X),
                           Id = atom_to_list(Y),
                              "<button type=\"submit\" name=add value="++Id++" class=\"btn btn-default btn-lg\"> "++Name ++"</button>"
                         end,NConnected),

  CButtons = lists:map(fun({Y,X}) ->
                           Name = atom_to_list(X),
                           IdL = atom_to_list(Y),
                              "<button type=\"submit\" name=remove value="++IdL++" class=\"btn btn-success btn-lg\"> "++ Name++"</button>"
                          end,lists:reverse(CIds)),

  WButton = case Status of
              undefined -> 
                "<button type=\"submit\" name=\"status\" value="++atom_to_list(Status)++" class=\"btn btn-default btn-lg\"> Water plant </button>";
              on ->
                "<button type=\"submit\" name=\"status\" value="++atom_to_list(Status)++" class=\"btn btn-danger btn-lg\"> Water plant </button>";
              off ->
                "<button type=\"submit\" name=\"status\" value="++atom_to_list(Status)++" class=\"btn btn-success btn-lg\"> Water plant </button>"
            end,
  [
   "<div class=\"container-fluid\">",
   side(),
   "<div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">
          <h1 class=\"page-header\">Settings ",PumpId,"</h1>
      <div class=\"row placeholders\" id=\"settings\">",
   "<p>Select the node the pump is attached to:</p>",
   "<form action=\"settings?pump=",PumpId,"\" method=\"post\" accept-charset=\"utf-8\">",
   NButtons,
   "</form>",
   "<form action=\"settings?pump=",PumpId,"\" method=\"post\" accept-charset=\"utf-8\">",
   CButtons,
   "</form>",
   "<p>Status: ",atom_to_list(Status),"</p>",
   "<form action=\"settings?pump=",PumpId,"\" method=\"post\" accept-charset=\"utf-8\">",
   WButton,
   "</form>",
   "</div>"
  ].

sensor(<<"POST">>, Node, Req0) ->

  NodeId = erlang:binary_to_atom(Node,utf8),
  Req3 = case cowboy_req:parse_header(<<"content-type">>, Req0) of 
           {<<"multipart">>, <<"form-data">>, _Data} ->
             {ok, _Headers, Req1} = cowboy_req:read_part(Req0),
             {ok, Data, Req2} = cowboy_req:read_part_body(Req1),

             Base64 = base64:encode(Data),
             plantsys_mng:set_image(NodeId,Base64), 
             Req2;

           _ -> 
             {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
             case hd(PostVals) of
               {<<"deletenode">>,_} -> 
                 plantsys_mng:remove_node(NodeId),
                 Req;
              _ -> 
               {_,NewName} = lists:keyfind(<<"newnode">>,1,PostVals),
               case NewName of 
                 <<>> -> undefined;
                 _ -> plantsys_mng:set_name(NodeId,binary_to_list(NewName))
               end,
               {_,NewLimit} = lists:keyfind(<<"newlimit">>,1,PostVals),
               case re:run(NewLimit,"^[0-9].*$") of
                 {match,_} -> 
                   Limit = erlang:binary_to_integer(NewLimit),
                   plantsys_mng:set_limit(NodeId,Limit);
                 _ -> undefined
               end,
               Req
           end
         end,
  cowboy_req:reply(302, #{
    <<"Location">> => <<"/">>
   }, Req3);


sensor(<<"GET">>, Node, Req0) ->
  NodeId = erlang:binary_to_atom(Node,utf8),
  {ok,RawData} = plantsys_mng:get_data(NodeId),
  Data = lists:map(fun(X) -> 
                D = binary_to_integer(maps:get(<<"data">>,X)),
                T = maps:get(<<"timestamp">>,X),
                [binary_to_integer(T),D]
            end,RawData),
  Title = "Settings",
  Nav = nav(),
  {ok,Image} = plantsys_mng:get_image(NodeId),
  {Head,Body} = case plantsys_mng:get_settings(NodeId) of 
                  {error,_ } -> "nothing";
                  {ok,Settings} ->  Limit = maps:get(limit,Settings),
                                    Name = maps:get(name,Settings),
                                    case Limit of 
                                      undefined -> 
                                        B = body(Node,Name,"\"no limit set\"",Image),
                                        H = head(io_lib:format("~w",[Data]),<<"0">>),
                                        {H,B};
                                      L -> 
                                        B = body(Node,Name,integer_to_list(L),Image),
                                        H = head(io_lib:format("~w",[Data]),erlang:integer_to_binary(Limit)),
                                        {H,B}
                                    end
                end,
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/html">>
   }, ["<html><head><title>", Title, "</title>",Head,"</head>",
       "<body><p>", Body,"</p>",Nav,"</body></html>"], Req0).

echo(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

head() -> 
  ["<link href=\"static/css/bootstrap.min.css\" rel=\"stylesheet\"> <link href=\"static/dashboard.css\" rel=\"stylesheet\"> <script src=\"/static/jquery.min.js\"></script>"].

head(DataPoints,Limit) -> 
  ["<link href=\"static/css/bootstrap.min.css\" rel=\"stylesheet\"> <link href=\"static/dashboard.css\" rel=\"stylesheet\"> <script src=\"/static/jquery.min.js\"></script>"
    ,"<script type=\"text/javascript\" src=\"/static/flot/jquery.flot.js\"></script>"
    ,"<script type=\"text/javascript\" src=\"/static/flot/jquery.flot.time.js\"></script>"
    ,"<script type=\"text/javascript\" src=\"/static/flot/jquery.flot.symbol.js\"></script>"
    ,"<script type=\"text/javascript\" src=\"/static/flot/jquery.flot.axislabels.js\"></script>"
    ,"<script>"
    ,"var markings = [ { yaxis: { from: 0, to: ",Limit," }, color: \"#FF6666\" }];"
    ,"var options = {grid: { markings: markings, backgroundColor: { colors: [\"#E6F9FF\",\"#96CBFF\"] } }, series: { lines: { show: true }, points: { show: true } }, yaxis:{ min:0 }, xaxis:{mode: \"time\" } };"
    ,"$(document).ready(function() { $.plot( $(\"#flot-placeholder\"), [ ",DataPoints," ], options); }); </script>"].  

nav() -> 
   "<nav class=\"navbar navbar-inverse navbar-fixed-top\">
      <div class=\"container-fluid\">
        <div class=\"navbar-header\">
          <button type=\"button\" class=\"navbar-toggle collapsed\" data-toggle=\"collapse\" data-target=\"#navbar\" aria-expanded=\"false\" aria-controls=\"navbar\">
            <span class=\"sr-only\">Toggle navigation</span>
            <span class=\"icon-bar\"></span>
          </button>
          <a class=\"navbar-brand\" href=\"/\">Cyber Plant Intelligence</a>
        </div>
        <div id=\"navbar\" class=\"navbar-collapse collapse\">
          <ul class=\"nav navbar-nav navbar-right\">
            <li><a href=\"#\">Dashboard</a></li>
            <li><a href=\"#\">Settings</a></li>
          </ul>
          <form class=\"navbar-form navbar-right\">
            <input type=\"text\" class=\"form-control\" placeholder=\"Search...\">
          </form>
        </div>
      </div>
    </nav>".

side() ->
[ "
<div class=\"col-sm-3 col-md-2 sidebar\">
  <ul class=\"nav nav-sidebar\">
    <li class=\"active\"><a href=\"#\">Overview <span class=\"sr-only\">(current)</span></a></li>
    <li><a href=\"#\">Reports</a></li>
    <li><a href=\"#\">Analytics</a></li>
    <li><a href=\"#\">Export</a></li>
  </ul>
  <ul class=\"nav nav-sidebar\">
    <li><a href=\"\">Nav item</a></li>
    <li><a href=\"\">Nav item again</a></li>
    <li><a href=\"\">One more nav</a></li>
    <li><a href=\"\">Another nav item</a></li>
    <li><a href=\"\">More navigation</a></li>
  </ul>
  <ul class=\"nav nav-sidebar\">
    <li><a href=\"\">Nav item again</a></li>
    <li><a href=\"\">One more nav</a></li>
    <li><a href=\"\">Another nav item</a></li>
  </ul>
</div>"
].

image(Image) ->
  ImageData = case Image of 
                undefined -> 
                 plot(); 
                Base64 -> 
                  ["<div class=\"row\"> <div class=\"col-xs-4\">", "<img height=\"40%\" src=\"data:image/jpeg;base64\,",Base64,"\">", "</div> <div class=\"col-xs-8\">", plot(), "</div> </div>"]
              end,
  ImageData.

body(Node,Name,Limit,Image) -> 
  PumpStatus = case plantsys_mng:get_pump(erlang:binary_to_atom(Node,utf8)) of 
                 {ok,undefined} -> 
                   <<"No pump connected">>;
                 {ok,R} -> 
                   erlang:atom_to_list(R)
               end,
  [
nav(),"
<div class=\"container-fluid\">
  <div class=\"row\">",side(),"
    <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">
      <h1 class=\"page-header\">Settings ",Name,"</h1>
      <div class=\"row placeholders\" id=\"settings\">
        <div class=\"container\">",image(Image),"
        </div>
        <form action=\"settings?node=",Node,"\" method=\"post\" accept-charset=\"utf-8\">
          <div class=\"input-group input-group-lg\">
            <span class=\"input-group-addon\" id=\"sizing-addon1\">Name</span>
            <input type=\"text\" class=\"form-control\" name=\"newnode\" placeholder=\"",Name,"\" aria-describedby= \"sizing-addon1\">
          </div>
          <br/>
          <p> Connected to pump:",PumpStatus," </p> 
          <div class=\"input-group input-group-lg\">
            <span class=\"input-group-addon\" id=\"sizing-addon1\">Limit</span>
            <input type=\"text\" class=\"form-control\" name=\"newlimit\" placeholder=",Limit," aria-describedby= \"sizing-addon1\">
          </div><br/>
          <div style=\"margin:1.5em 0 1.5em 0\">
            <button type=\"submit\" class=\"btn btn-primary btn-block\">Submit</button>
          </div>
        </form>
        <form action=\"settings?node=",Node,"\" method=\"post\" enctype=\"multipart/form-data\" accept-charset=\"utf-8\">
          <div class=\"input-group input-group-lg\">
            <span class=\"input-group-addon\" id=\"sizing-addon1\">Picture</span>
            <input type=\"file\" accept=\"image/*\" class=\"form-control\" name=\"newimage\" placeholder=\"Image\" aria-describedby= \"sizing-addon1\">
            <button type=\"submit\" class=\"btn btn-primary btn-block\">Upload</button>
          </div>
        </form>
        <form action=\"settings?node=",Node,"\" method=\"post\" accept-charset=\"utf-8\">
          <button type=\"submit\" name=\"deletenode\" class=\"btn btn-danger btn-lg pull-left\"><span class=\"glyphicon glyphicon-warning-sign\" aria-hidden=\"true\"></span>  Delete</button>
        </form>
        </div>
      </div>
    </div>
  </div>
</div>
 " ].

plot() -> 
  [ "<div id=\"flot-placeholder\" style=\"width:100%;height:400px\"></div>"].
