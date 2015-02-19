-module(rgm_server_interface_app).
-author("kei 2015-02-18").

-behaviour(application).

-export([
    start/2,
    stop/1
]).

-define(DEFAULT_PORT, 1024).

start(_StartType, _StartArgs) ->
    case application:get_env(rgm_server_interface, port) of
        {ok, P} ->
            Port = P;
        undefined ->
            Port = ?DEFAULT_PORT
    end,
    case rgm_server_interface_sup:start_link(Port) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.