-module(settings).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{node := Node,pump:=Pump,leds:=Leds} = cowboy_req:match_qs([{node, [], undefined},{pump, [], undefined},{leds, [], undefined}], Req0),
  Req = case {Node,Pump,Leds} of
          {undefined,undefined,undefined} -> 
            echo(Method,undefined,Req0);
          {undefined,_Pump,undefined} -> 
            pump(Method, Pump, Req0);
          {_Node,undefined,undefined} -> 
            sensor(Method, Node, Req0);
          {undefined,undefined,Leds} -> 
            leds(Method,Leds,Req0)
        end,
  {ok, Req, Opts}.

pump_action([{<<"timerwait">>,WaitTimer},{<<"timerrun">>,RunTimer},{<<"starttimer">>,Status}|_],Pump) -> 
  PumpId = binary_to_atom(Pump,utf8),
  case Status of
    <<"on">> ->
      plantsys_usrmng:stop_pumptimer(vikotr,PumpId);
    <<"off">> -> 
      WTimerMin = erlang:binary_to_integer(WaitTimer)*1000*60*60,
      RTimerSec = erlang:binary_to_integer(RunTimer)*1000,
      plantsys_usrmng:start_pumptimer(viktor,PumpId,WTimerMin,RTimerSec)
  end;

pump_action([{<<"add">>,Node}|_],Pump) -> 
  NodeId = binary_to_atom(Node,utf8),
  PumpId = binary_to_atom(Pump,utf8),
  plantsys_usrmng:set_pump(viktor,NodeId,PumpId);

pump_action([{<<"remove">>,Node}|_],Pump) -> 
  NodeId = binary_to_atom(Node,utf8),
  PumpId = binary_to_atom(Pump,utf8),
  plantsys_usrmng:remove_pumpnode(viktor,PumpId,NodeId);

pump_action([{<<"status">>,Current}|_],Pump) -> 
  PumpId = binary_to_atom(Pump,utf8),
  case Current of
    <<"undefined">> -> 
      plantsys_usrmng:start_pump(viktor,PumpId);
    <<"on">> -> 
      plantsys_usrmng:stop_pump(viktor,PumpId);
    <<"off">> -> 
      plantsys_usrmng:start_pump(viktor,PumpId)
  end.

pump(<<"POST">>, Pump, Req0) ->
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  io:format("PostVals:~p",[PostVals]),
  pump_action(PostVals,Pump),
  
  cowboy_req:reply(302, #{
    <<"Location">> => <<"/settings?pump=",Pump/binary>>
   }, Req);

pump(<<"GET">>, Pump, Req0) ->
  PumpId = erlang:binary_to_atom(Pump,utf8),
  {ok,PumpData} = plantsys_usrmng:get_pumpdata(viktor,PumpId),
  Connected = maps:get(nodes,PumpData),
  Status = maps:get(status,PumpData),
  Timer = maps:get(timer,PumpData),
  Title = "Pump Settings",
  Body = pump_body(Pump,Status,Connected,Timer), 
  Head = head(),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/html">>
   }, ["<html><head><title>", Title, "</title>",Head,"</head>",
       "<body>",Body,"</body></html>"], Req0).
   
pump_body(PumpId,Status,Connected,Timer) -> 
  {ok,Nodes} = plantsys_usrmng:get_nodes(viktor),
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
  TimerNext = "<script>
  if("++integer_to_list(maps:get(next,Timer))++"!=0){
    var d = new Date("++integer_to_list(maps:get(next,Timer)*1000)++");
    document.write(\"Pump next time:\"+d.toString());
  }
  </script>",
  TimerFileds = 
  [" <div class=\"col-sm-8\">
      <div class=\"input-group \">
        <label for=\"timewait\" class=\"col-sm-2 control-label\">Timer interval</label>
        <span class=\"input-group-addon\" id=\"basic-addon1\">hours [1-24]</span>
        <input id=\"timewait\" type=\"number\" min=\"1\" max=\"24\" name=\"timerwait\" value=\""++integer_to_list(maps:get(tw,Timer) div (1000*60*60))++"\" class=\"form-control\" placeholder=\"min\" aria-describedby=\"basic-addon1\">
      </div>
      <div class=\"input-group \">
        <label for=\"timewait\" class=\"col-sm-2 control-label\">Pump time</label>
        <span class=\"input-group-addon\" id=\"basic-addon1\">seconds [1-60]</span>
        <input type=\"number\" min=\"1\" max=\"60\" name=\"timerrun\" value=\""++integer_to_list(maps:get(tr,Timer) div (1000))++"\" class=\"form-control\" placeholder=\"min\" aria-describedby=\"basic-addon1\">
      </div>
        <span class=\"input-group-btn\">",
       case maps:get(ts,Timer) of 
          off -> 
          "<button type=\"submit\" name=\"starttimer\" value=\"off\" class=\"btn btn-success\">Start timer</button>";
          _ -> 
          "<button type=\"submit\" name=\"starttimer\" value=\"on\" class=\"btn btn-danger\">Stop timer</button>"
        end,
        "</span> 
     </div>" ],
  [
   nav(),"<div class=\"container-fluid\">",
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
   
   "<span id=\"next_timer\">"++TimerNext++"</span>",
   "<form action=\"settings?pump=",PumpId,"\" method=\"post\" accept-charset=\"utf-8\">",
   TimerFileds,
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
             plantsys_usrmng:set_image(viktor,NodeId,Base64), 
             Req2;

           _ -> 
             {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
             case hd(PostVals) of
               {<<"deletenode">>,_} -> 
                 plantsys_usrmng:remove_node(viktor,NodeId),
                 Req;
              _ -> 
               {_,NewName} = lists:keyfind(<<"newnode">>,1,PostVals),
               case NewName of 
                 <<>> -> undefined;
                 _ -> plantsys_usrmng:set_name(viktor,NodeId,binary_to_list(NewName))
               end,
               {_,NewLimit} = lists:keyfind(<<"newlimit">>,1,PostVals),
               case re:run(NewLimit,"^[0-9].*$") of
                 {match,_} -> 
                   Limit = erlang:binary_to_integer(NewLimit),
                   plantsys_usrmng:set_limit(viktor,NodeId,Limit);
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
  {ok,RawData} = plantsys_usrmng:get_data(viktor,NodeId),
  Data = lists:map(fun(X) -> 
                D = binary_to_integer(maps:get(<<"data">>,X)),
                T = maps:get(<<"timestamp">>,X),
                [binary_to_integer(T),D]
            end,RawData),
  Title = "Settings",
  Nav = nav(),
  {ok,Image} = plantsys_usrmng:get_image(viktor,NodeId),
  {Head,Body} = case plantsys_usrmng:get_settings(viktor,NodeId) of 
                  {error,_ } -> "nothing";
                  {ok,Settings} ->  Limit = maps:get(limit,Settings),
                                    Name = maps:get(name,Settings),
                                    Last = maps:get(last_water,Settings),
                                    case Limit of 
                                      undefined -> 
                                        B = body({Node,Last},Name,"\"no limit set\"",Image),
                                        H = head(io_lib:format("~w",[Data]),<<"0">>),
                                        {H,B};
                                      L -> 
                                        B = body({Node,Last},Name,integer_to_list(L),Image),
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
  ["<link href=\"static/css/bootstrap.min.css\" rel=\"stylesheet\"> <link href=\"static/dashboard.css\" rel=\"stylesheet\"> <script src=\"/static/jquery.min.js\"></script>",
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" ].

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

pumpstatus(Pump,Last) ->
  R = case Pump of
        <<"No pump connected">> -> 
          ["<div class=\"alert alert-warning\" role=\"alert\">",Pump,"</div>"];
        _ ->
          case Last of 
            undefined -> 
              ["<div class=\"alert alert-success\" role=\"alert\"> Pump: <strong>",Pump,"</strong> is connected</div>"];
            #{time:=LastW} -> 

              TimerNext = "<script>
                var d = new Date("++integer_to_list(LastW*1000)++");
                document.write(d.toString());
              </script>",
              ["<div class=\"alert alert-success\" role=\"alert\"> Pump: <strong>",Pump,"</strong> is connected, last watered ",TimerNext,"</div>"]

          end
      end,
  io:format("Pumpstatus: ~p~n",[R]),
  R.

int_to_hex(C) ->
    CHex = integer_to_list(C,16),
    case length(CHex) of
        1 -> "0"++CHex;
        _ -> CHex
    end.



    


body({Node,Last},Name,Limit,Image) -> 
    {ok,Leds} = plantsys_usrmng:get_leds(viktor),
    LedsStatus = lists:map(fun(Led) -> 
                                   {ok,LedData} = plantsys_usrmng:get_led(viktor,maps:get(id,Led)),
                                   LedNodes = maps:get(nodes,LedData),
                                   Color = maps:get(color,LedData),
                                   R = maps:get(r,Color),
                                   G = maps:get(g,Color),
                                   B = maps:get(b,Color),
                                   CHex = int_to_hex(R)++int_to_hex(G)++int_to_hex(B), 
                                   case lists:keyfind(binary_to_atom(Node,utf8),1,LedNodes) of 
                                       {_,_} -> 
                                           "<a href=\"settings?leds="++ erlang:atom_to_list(maps:get(id,Led)) ++"\" class=\"btn btn-success\" role=\"button\">
                                           <span class=\"glyphicon glyphicon-tint\"style=\"color:#"++CHex++"\"></span> "++maps:get(name,Led) ++"</a>";
                                       false -> 
                                           "<a href=\"settings?leds="++ erlang:atom_to_list(maps:get(id,Led)) ++"\" class=\"btn btn-danger\" role=\"button\">"++maps:get(name,Led) ++"</a>"
                                   end
                           end,Leds),


  PumpStatus = case plantsys_usrmng:get_connected_pump(viktor,erlang:binary_to_atom(Node,utf8)) of 
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
          <br/><div class=\"input-group input-group-lg\">
            <span class=\"input-group-addon\" id=\"sizing-addon1\">Limit</span>
            <input type=\"text\" class=\"form-control\" name=\"newlimit\" placeholder=",Limit," aria-describedby= \"sizing-addon1\">
          </div><br/> ",
          pumpstatus(PumpStatus,Last)
          ,"
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
        <h2>Leds:</h2>",
          LedsStatus,
        "<form action=\"settings?node=",Node,"\" method=\"post\" accept-charset=\"utf-8\">
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

leds(<<"POST">>, Leds, Req0) ->
  LedsId = erlang:binary_to_atom(Leds,utf8),
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  case PostVals of 
    [{<<"sh">>,SH}, {<<"sm">>,SM}, {<<"eh">>,EH}, {<<"em">>,EM}] ->
      plantsys_usrmng:set_ledstimer(viktor,LedsId,binary_to_integer(SH),binary_to_integer(SM),binary_to_integer(EH),binary_to_integer(EM));
    [{<<"color">>,CHexBin}] -> 
         CHex = binary_to_list(CHexBin), 
         R = list_to_integer(lists:sublist(CHex,1,2),16),
         G = list_to_integer(lists:sublist(CHex,3,2),16),
         B = list_to_integer(lists:sublist(CHex,5,2),16),
      plantsys_usrmng:set_ledscolor(viktor,LedsId,R,G,B);
      [{<<"node">>,Node}] -> 
          NodeId = binary_to_atom(Node,utf8),
          {ok,NData}= plantsys_usrmng:get_led(viktor,LedsId),
          case lists:keyfind(NodeId,1,maps:get(nodes,NData)) of 
              {_,_} ->
                  plantsys_usrmng:unset_leds(viktor,NodeId,LedsId);
              false ->
                  plantsys_usrmng:set_leds(viktor,NodeId,LedsId)
          end
  end,
  
  cowboy_req:reply(302, #{
    <<"Location">> => <<"/settings?leds=",Leds/binary>>
   }, Req);


leds(<<"GET">>, Leds, Req0) ->
  LedsId = erlang:binary_to_atom(Leds,utf8),
  {ok,Color} = plantsys_usrmng:get_ledscolor(viktor,LedsId),
  Title = "Light color",
  Body = leds_body(Leds,Color), 
  Head = head(),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/html">>
   }, ["<html><head><title>", Title, "</title>",Head,"</head>",
       "<body>",Body,"</body></html>"], Req0).



leds_body(Leds,Color) -> 
  {ok, Data} = plantsys_usrmng:get_led(viktor,binary_to_atom(Leds,utf8)),
  {ok, Nodes} = plantsys_usrmng:get_nodes(viktor),
  ButtonNodes = lists:map(fun(Node) ->
                                  case lists:keyfind(maps:get(id,Node),1,maps:get(nodes,Data)) of 
                                      {_,_} -> 
                                        "<button type=\"submit\" name=\"node\" value="++atom_to_list(maps:get(id,Node))++" class=\"btn btn-success btn-lg\">"++maps:get(name,Node)++"</button>";
                                      false ->
                                        "<button type=\"submit\" name=\"node\" value="++atom_to_list(maps:get(id,Node))++" class=\"btn btn-danger btn-lg\">"++maps:get(name,Node)++"</button>"
                                  end

                          end,Nodes),
  R =integer_to_list(maps:get(r,Color)),
  G =integer_to_list(maps:get(g,Color)),
  B =integer_to_list(maps:get(b,Color)),

  CHex = int_to_hex(list_to_integer(R))++int_to_hex(list_to_integer(G))++int_to_hex(list_to_integer(B)),

  {{SH,SM},{EH,EM}} = case plantsys_usrmng:get_ledstimer(viktor,erlang:binary_to_atom(Leds,utf8)) of 
                        {ok,undefined} -> {{0,0},{0,0}};
                        {ok,Timer} -> Timer
                      end,
  [
   nav(),"<div class=\"container-fluid\">",
   side(),
   "<div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">
          <h1 class=\"page-header\">Settings ",Leds,"</h1>
      <div class=\"row placeholders\" id=\"settings\">
    <form action=\"settings?leds=",Leds,"\" method=\"post\" accept-charset=\"utf-8\">
     Color:#<input type=\"text\" name=\"color\", value="++CHex++"><br>
    <button type=\"submit\" class=\"btn btn-success\">Set Color</button>
   </form>
   <u><h1>Timer</h1></u>
    <form action=\"settings?leds=",Leds,"\" method=\"post\" accept-charset=\"utf-8\">
    <h2>Start sleep</h2>
     <input type=\"text\" name=\"sh\", value="++integer_to_list(SH)++"> : 
     <input type=\"text\" name=\"sm\", value="++integer_to_list(SM)++"><br>
    <h2>Stop sleep</h2>
     <input type=\"text\" name=\"eh\", value="++integer_to_list(EH)++"> : 
     <input type=\"text\" name=\"em\", value="++integer_to_list(EM)++"><br>
    <button type=\"submit\" class=\"btn btn-success\">Set Timer</button>
   </form>
    <h1> Nodes Connected: </h1>
    <form action=\"settings?leds=",Leds,"\" method=\"post\" accept-charset=\"utf-8\">",ButtonNodes,
   "</form>

   </div>"
  ].

