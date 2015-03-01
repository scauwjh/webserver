-module(rd_app).
-author("kei 2015-02-28").
-description("Resource discovery app, for synchronizing the datas").

%% app API
-export([
	start/2,
	stop/1
]).

%% to start the resource_discovery app
start(_StartType, _StartArgs) ->
	io:format("starting resource_discovery...~n"),
	case rd_sup:start_link() of
	{ok, Pid} ->
		{ok, Pid};
	Other ->
		{error, Other}
	end.

stop(_State) ->
	ok.
