-module(server).
-author("kei 2015-03-03").
-description("Entrance of the web server").

-behaviour(application).

-export([
    start/2,
    stop/1
]).

-define(DEFAULT_PORT, 1024).

start(_StartType, _StartArgs) ->
	io:format("starting server...~n"),
	case application:get_env(server, port) of
	{ok, P} ->
		Port = P;
	undefined ->
		Port = ?DEFAULT_PORT
	end,
    %% start rgm_server
	rgm_server_interface_sup:start_link(Port),
	%% start resource init
	resource_init_sup:start_link().

stop(_State) ->
    ok.