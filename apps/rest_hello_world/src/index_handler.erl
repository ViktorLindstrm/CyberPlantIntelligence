
%% @doc Cookie handler.
-module(index_handler).

-export([init/2]).

init(Req0, Opts) ->
    Cookies = cowboy_req:parse_cookies(Req0),
    logger:debug("Cookies: ~p~n",[Cookies]),
    Req1 = case lists:keyfind(<<"sessionid">>, 1,Cookies) of 
               {_,SessionID} -> 
                   case plantsys_usrmng:validate_user(SessionID) of 
                       {ok,_User} -> 
                           logger:debug("User Found"),
                           Body = body(),
                           cowboy_req:reply(200, #{
                             <<"content-type">> => <<"text/html">>
                            }, Body, Req0);

                       {error,E} ->
                           logger:error("Error: ~p~n",[E]),
                           Req = cowboy_req:reply(302, #{
                                   <<"Location">> => <<"/login">>
                                  }, Req0),
                           {ok, Req, Opts}
                   end;

               false -> 
                   cowboy_req:reply(302, #{
                     <<"Location">> => <<"/login">>
                    }, Req0)
           end,
    {ok, Req1, Opts}.

head() ->
["<head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <meta name=\"description\" content=\"\">
    <meta name=\"author\" content=\"\">

    <title>Dashboard Template for Bootstrap</title>

    <!-- Bootstrap core CSS -->
    <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" integrity=\"sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u\" crossorigin=\"anonymous\">

    <!-- Custom styles for this template -->
    <link href=\"/static/dashboard.css\" rel=\"stylesheet\">

    <script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\"
       integrity=\"sha256-3edrmyuQ0w65f8gfBsqowzjJe2iM6n0nKciPUp8y+7E=\"
       crossorigin=\"anonymous\"></script>

    <script type=\"text/javascript\">
      
      var websocket;
      $(document).ready(init);
      
      function init() {
          if(!(\"WebSocket\" in window)){  
              $('#status').append('<p><span style=\"color: red;\">websockets are not supported </span></p>');
              $(\"#navigation\").hide();  
          } else {
              connect();
          };
      };

      function connect()
      {   wsHost = \"ws://\" + window.location.host + \"/websocket\";
          websocket = new WebSocket(wsHost);
          websocket.onopen = function(evt) { onOpen(evt) }; 
          websocket.onclose = function(evt) { onClose(evt) }; 
          websocket.onmessage = function(evt) { onMessage(evt) }; 
          websocket.onerror = function(evt) { onError(evt) }; 
      };  
      
      function disconnect() {
          websocket.close();
      }; 

      function toggle_connection(){
          if(websocket.readyState == websocket.OPEN){
              disconnect();
          } else {
              connect();
          };
      };

      function sendTxt() {
          if(websocket.readyState == websocket.OPEN){
              txt = $(\"#send_txt\").val();
              websocket.send(txt);
              showScreen('sending: ' + txt); 
          } else {
               showScreen('websocket is not connected'); 
          };
      };

      function onOpen(evt) { 
        //fetch nodes 
        websocket.send('nodes');
        websocket.send('pumps');
      };  

      function onClose(evt) { 
          showScreen('<span style=\"color: red;\">DISCONNECTED </span>');
      };  

      function onMessage(evt) { 
          console.log(evt.data);
          if(evt.data.substring(0,5) == \"Nodes\"){
            var nodes = JSON.parse(evt.data.substring(6));
            nodehtml =\"\";
            for(i=0;i<nodes.length;i++){
              name = String.fromCharCode.apply(this, nodes[i].name)
              current = nodes[i].current;
              progressbar=\"progress-bar-info\";
              if(isNaN(nodes[i].limit)){
                progressbar = \"progress-bar-info progress-bar-striped\";
              }
              else if(current.data<nodes[i].limit){
                progressbar = \"progress-bar-danger\";
              }
              nodehtml += \"<div class=\\\"col-xs-6 col-sm-3 placeholder\\\">\"+
                \"<a href=\\\"/settings?node=\"+nodes[i].id+\"\\\" >\"+
                
                \"<div class=\\\"containe\\\">\"+
                  \"<div class=\\\"progress vertical\\\">\"+
                    \"<div class=\\\"progress-bar \"+progressbar+\"\\\"\"+
                    \"role=\\\"progressbar\\\" aria-valuenow=\\\"90\\\" aria-valuemin=\\\"0\\\"\"+
                    \"aria-valuemax=\\\"100\\\" style=\\\"width:\"+current.data +\"%;\\\">\"+
                    \"</div> \"+
                  \"</div>\"+
                \"</div>\"+
                \"</a>\"+
                \"<h4>\" + name + \"</h4>\"+
                \"<span class=\\\"text-muted\\\">\"+current.data+\" / \"+nodes[i].limit+\"</span>\"+ \"</div>\"

              $('#nodes').html(nodehtml);
            }
          } else if(evt.data.substring(0,5) == \"Pumps\"){
            var pumps = JSON.parse(evt.data.substring(6));
            pumphtml = \"\";
            for(i=0;i<pumps.length;i++){
            pumphtml+=\"<div class=\\\"col-xs-6 col-sm-3 \\\">\"+
                        \"<a href=\\\"/settings?pump=\"+pumps[i].id+\"\\\" >\"+
                        \"<img src=\\\"/static/img/pump.png\\\" width=100 >\"+
                        \"</a>\"+
                        \"<h4>\" + pumps[i].id + \"</h4>\"+
                      \"</div>\";
            }
            $('#pumps').html(pumphtml);
          }
      };  

      function onError(evt) {
          showScreen('<span style=\"color: red;\">ERROR: ' + evt.data+ '</span>');
      };

      function showScreen(txt) { 
          $('#output').prepend('<p>' + txt + '</p>');
      };

      function clearScreen() 
      { 
          $('#output').html(\"\");
      };
    </script>

  </head>"
].
body() ->

["<!DOCTYPE html>
<html lang=\"en\">",
 head(),"
  <body>
    <nav class=\"navbar navbar-inverse navbar-fixed-top\">
      <div class=\"container-fluid\">
        <div class=\"navbar-header\">
          <button type=\"button\" class=\"navbar-toggle collapsed\" data-toggle=\"collapse\" data-target=\"#navbar\" aria-expanded=\"false\" aria-controls=\"navbar\">
            <span class=\"sr-only\">Toggle navigation</span>
            <span class=\"icon-bar\"></span>
            <span class=\"icon-bar\"></span>
            <span class=\"icon-bar\"></span>
          </button>
          <a class=\"navbar-brand\" href=\"#\">Cyber Plant Intelligence</a>
        </div>
        <div id=\"navbar\" class=\"navbar-collapse collapse\">
          <ul class=\"nav navbar-nav navbar-right\">
            <li><a href=\"#\">Dashboard</a></li>
            <li><a href=\"#\">Settings</a></li>
            <li><a href=\"/user\">Profile</a></li>
            <li><a href=\"#\">Help</a></li>
          </ul>
          <form class=\"navbar-form navbar-right\">
            <input type=\"text\" class=\"form-control\" placeholder=\"Search...\">
          </form>
        </div>
      </div>
    </nav>

    <div class=\"container-fluid\">
      <div class=\"row\">
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
        </div>
        <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">
          <h1 class=\"page-header\">Dashboard</h1>
          <div id=\"navigation\">
            <div id=\"connected\">				
            </div>
          </div>
          <div class=\"panel panel-default\">
            <div class=\"panel-heading\"><center><b>Plants</b></center></div>
              <div class=\"panel-body\">

                <div class=\"row placeholders\" id=\"nodes\">
              </div>
            </div>
          </div>
          <div class=\"panel panel-default\">
            <div class=\"panel-heading\"><center><b>Pumps</b></center></div>
              <div class=\"panel-body\">
                <div class=\"row placeholders\" id=\"pumps\">
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    <!-- Bootstrap core JavaScript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    <script>window.jQuery || document.write('<script src=\"js/vendor/jquery.min.js\"><\\/script>')</script>
    <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js\" integrity=\"sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa\" crossorigin=\"anonymous\"></script>
  </body>
</html>"].
