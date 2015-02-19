-module(main).
-author("kei 2015-02-15").
-description("main function, for starting server").

%% Include files
-include("common.hrl").

%% Exports
-export([
    start/0
]).

%% External functions
start() ->
	io:format("Starting server...~n"),
	start_http_interface().

start_http_interface() ->
	application:start(rgm_server_interface).

