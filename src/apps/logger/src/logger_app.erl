-module(logger_app).
-author("kei 2015-03-05").

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    io:format("starting logger...~n"),
    %% start supervisor
    case logger_sup:start_link() of
    {ok, Pid} ->
        logger_handler:add_handler(),
        {ok, Pid};
    Other ->
        logger:error("start logger failed!", Other),
        {error, Other}
    end.
stop(_State) ->
    ok.
