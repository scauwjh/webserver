-module(cache_sup).
-author("kei 2015-02-27").
-description("Supervisor of cache, one element for one data").

-behaviour(supervisor).

%% supervisor callbacks
-export([
    init/1
]).

%% API
-export([
    start_link/0,
    start_child/2
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?MODULE, [Value, LeaseTime]).

init([]) ->
    Element = {
        cache_element,
        {cache_element, start_link, []},
        temporary, brutal_kill, worker, [cache_element]
    },
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.