-module(server).
-author("kei 2015-03-03").
-description("Entrance of the web server").

-behaviour(application).

-include("server.hrl").

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    io:format("starting server...~n"),
    case application:get_env(server, port) of
    {ok, P} ->
        Port = P;
    undefined ->
        Port = ?DEFAULT_PORT
    end,
    case server_sup:start_link(Port) of
    {ok, Pid} ->
        {ok, Pid};
    {error, Other} ->
        logger:error("start server failed!", Other),
        {error, Other}
    end.

stop(_State) ->
    ok.