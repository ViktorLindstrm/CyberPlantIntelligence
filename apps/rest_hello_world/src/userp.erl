-module(userp).

-export([init/2]).
-record(user, {node_token,sessionid,access_token,id,mng,sup,username}).

%-record(user, {sessionid,access_token,id,mng,sup,username}).

init(Req0, Opts) ->
    Cookies = cowboy_req:parse_cookies(Req0),
    Req = case lists:keyfind(<<"sessionid">>, 1,Cookies) of
              {_,SessionID} ->
                  case plantsys_usrmng:validate_user(SessionID) of
                      {ok,User} ->
						  UserID = User#user.username,
                            logger:debug("UserId: ~p~n",[UserID]),
                            NodeToken = case plantsys_usrmng:get_node_token(UserID) of
                                          {ok,undefined} -> 
                                              {ok,T} = plantsys_usrmng:generate_node_token(UserID),
                                              T;
                                          {ok,T} ->  
                                              T
                                      end,
                          Title = "User Settings",
                          Body = body(atom_to_list(UserID),NodeToken),
                          Head = head(),

                          cowboy_req:reply(200, #{
                            <<"content-type">> => <<"text/html">>
                           }, ["<html><head><title>", Title, "</title>",Head,"</head>",
                               "<body>",Body,"</body></html>"], Req0);
                      {error,_} ->
                          cowboy_req:reply(302, #{
                            <<"Location">> => <<"/login">>
                           }, Req0)
                  end;
              false ->
                  cowboy_req:reply(302, #{
                    <<"Location">> => <<"/login">>
                   }, Req0)
          end,

    {ok, Req, Opts}.


head() ->
["<head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <meta name=\"description\" content=\"\">
    <meta name=\"author\" content=\"\">

    <title>User config</title>

    <!-- Bootstrap core CSS -->
    <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" integrity=\"sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u\" crossorigin=\"anonymous\">

    <!-- Custom styles for this template -->
    <link href=\"/static/dashboard.css\" rel=\"stylesheet\">

    <script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\"
       integrity=\"sha256-3edrmyuQ0w65f8gfBsqowzjJe2iM6n0nKciPUp8y+7E=\"
       crossorigin=\"anonymous\"></script>

  </head>"].

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
            <li><a href=\"/user\">Profile</a></li>
            <li><a href=\"#\">Help</a></li>
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

body(Name,Token) ->
    [
     nav(),"
     <div class=\"container-fluid\">
       <div class=\"row\">",side(),"
         <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">
           <h1 class=\"page-header\">Settings ",Name,"</h1>
           <div class=\"row placeholders\" id=\"settings\">
             <div class=\"container\">
                  <p>Token: <b>",Token,"</b></p>
             </div>
             </div>
           </div>
         </div>
       </div>
     </div>
 " ].
