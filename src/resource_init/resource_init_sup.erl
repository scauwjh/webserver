-module(resource_init_sup).
-author("kei 2015-03-03").

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% Supervisor callbacks
-export([
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Init = {
        resource_init,
        {resource_init, start_link, []},
        permanent, 2000, worker, [resource_init]
    },
    Children = [Init],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.