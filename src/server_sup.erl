-module(server_sup).
-author("kei 2015-03-06").

-behaviour(supervisor).

%% API
-export([
	start_link/1
]).

%% Supervisor callbacks
-export([
	init/1
]).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    Resource = {
        resource_init_sup,
        {resource_init_sup, start_link, []},
        permanent, 2000, supervisor, [resource_init_sup]
    },
    Server = {
    	rgm_server_interface_sup,
    	{rgm_server_interface_sup, start_link, [Port]},
    	permanent, 2000, supervisor, [rgm_server_interface_sup]
    },
    % Acceptor =
    Children = [Resource, Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.