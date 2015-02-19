-module(rgm_server_interface_sup).
-author("kei 2015-02-17").

-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_child/0
]).

%% Supervisor callbacks
-export([
    init/1
]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([Port]) ->
    Server = {
    	rgm_server_interface,
    	{rgm_server_interface, start_link, [Port]},
		permanent, 2000, worker, [rgm_server_interface]
	},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.