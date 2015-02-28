-module(rd_sup).
-author("kei 2015-02-28").
-description("Resource discovery app supervisor").

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
	Server = {
		rd_server,
		{rd_server, start_link, []},
		permanent, 2000, work, [rd_server]
	},
	Children = [Server],
	RestarStrategy = {one_for_one, 0, 1},
	{ok, {RestarStrategy, Children}}.
