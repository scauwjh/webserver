-module(rgm_connection_sup).
-author("kei 2015-02-17").
-description("web server connection supervisor").

-behaviour(supervisor).

%% supervisor callbacks
-export([
	init/1
]).

-export([
	start_link/4,
	start_child/1
]).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link(Callback, IP, Port, UserArgs) ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE,
		[Callback, IP, Port, UserArgs]),
	start_child(Pid),
	{ok, Pid}.

start_child(Pid) ->
	supervisor:start_child(Pid, []).

init([Callback, IP, Port, UserArgs]) ->
	BasicSocketOpts = [binary, {active, false}, {packet, http_bin},
		{reuseaddr, true}],
	SockOpts = case IP of
		undefined ->
			BasicSocketOpts;
		_ ->
			[{ip, IP} | BasicSocketOpts]
		end,
	{ok, LSock} = gen_tcp:listen(Port, SockOpts),
	Server = {
		rgm_server_worker,
		{rgm_server_worker, start_link, [Callback, LSock, UserArgs]},
		temporary, brutal_kill, worker,[rgm_server_worker]
	},
	RestarStrategy = {simple_one_for_one, 1000, 3600},
	Children = [Server],
	{ok, {RestarStrategy, Children}}.



