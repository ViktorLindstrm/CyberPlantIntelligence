
%% @doc Cookie handler.
-module(login).

-export([init/2]).

init(Req0, Opts) ->

    Cookies = cowboy_req:parse_cookies(Req0),
    SessionID = case lists:keyfind(<<"sessionid">>, 1,Cookies) of 
                    {_,SID} -> 
                        SID;
                    false -> 
                        base64:encode(crypto:strong_rand_bytes(32))
                end,
    logger:debug("SessionID: ~p~n",[SessionID]),

    Req = case plantsys_usrmng:validate_user(SessionID) of 
              {ok,_User} -> 
                  cowboy_req:reply(302, #{
                    <<"Location">> => <<"/">>
                   }, Req0);
              {error,_} ->
                  logger:error("User not found~n"),
                  #{code:=Code} = cowboy_req:match_qs([{code, [], undefined}], Req0),
                  not_loggedin(Req0,Code,SessionID)
          end,
    {ok, Req, Opts}.

not_loggedin(Req0,Code,SessionID) ->
    Req = case Code of
              undefined -> 
                  Req1 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0,#{http_only => true}),
                  Body = ["<html><body><a href=\"http://127.0.0.1:8180/authorize?response_type=code&client_id=cpi&state=xyz&redirect_uri=http://127.0.0.1:8080/login\">Login</a></body></html>"],
                  cowboy_req:reply(200, #{
                    <<"content-type">> => <<"text/html">>
                   }, Body, Req1);
              C -> 
                  Client = "cpi",
                  {ok,Pass} = application:get_env(rest_hello_world,client_secret),
                  logger:debug("Client secret: ~p~n",[Pass]),

                  Auth = "Basic "++binary_to_list(base64:encode(Client++":"++Pass)),
                  Body = "grant_type=authorization_code&code="++binary_to_list(C)++"&redirect_uri=http://127.0.0.1:8080/login&client_id=cpi",
                  Request = {"http://127.0.0.1:8180/token", [{"Authorization",Auth}], [], Body},

                  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Bdy}} = httpc:request(post, Request, [], []),

                  {Token} = jiffy:decode(Bdy),  
                  {_,AccessToken} = lists:keyfind(<<"access_token">>,1,Token),
                  Cookies = cowboy_req:parse_cookies(Req0),
                  {_,CSessionID} = lists:keyfind(<<"sessionid">>, 1,Cookies),
                  plantsys_usrmng:login_user(AccessToken,CSessionID),
                  cowboy_req:reply(302, #{
                    <<"Location">> => <<"/">>
                   }, Req0)
          end,
    Req.
