-module(logger_sup).
-author("kei 2015-03-05").

-behaviour(supervisor).

%% supervisor callbacks
-export([
    init/1
]).

%% API
-export([
    start_link/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Logger = {
        logger,
        {logger, start_link, []},
        temporary, brutal_kill, worker, [logger]
    },
    Children = [Logger],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.