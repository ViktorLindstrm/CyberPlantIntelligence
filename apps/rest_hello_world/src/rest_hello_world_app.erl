%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/sensor/:token/:id", toppage_handler, []},
                                             {"/pump/:token/:id", pump_handler, []},
                                             {"/leds/:token/:id", leds_handler, []},
                                             {"/settings", settings, []},
                                             {"/login", login, []},
                                             {"/user", userp, []},
                                             {"/", index_handler, []},
                                             %{"/test", cowboy_static, {priv_file, rest_hello_world, "index.html"}},
                                             {"/node", cowboy_static, {priv_file, rest_hello_world, "get.html"}},
                                             {"/websocket", ws_handler,[]},
											 {"/static/[...]", cowboy_static, {priv_dir, rest_hello_world, "static"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
                                           env => #{dispatch => Dispatch}
                                          }),
    rest_hello_world_sup:start_link().

stop(_State) ->
    ok.
