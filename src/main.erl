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
    io:format("rgm_server v1.0.0~n"),
    start_apps().

start_apps() ->
    application:start(logger),
    application:start(resource_discovery),
    application:start(cache),
    application:start(server).

