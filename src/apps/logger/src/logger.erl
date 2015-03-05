-module(logger).
-author("kei 2015-03-05").
-description("Simple logger for web server").

-export([
	start_link/0,
	debug/1,
	info/1,
	error/1,
	add_handler/2,
	delete_handler/2
]).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link() ->
	gen_event:start_link({local, ?MODULE}).

%% debug log
debug(Value) ->
	gen_event:notify(?MODULE, {debug, Value}).

%% info log
info(Value) ->
	gen_event:notify(?MODULE, {info, Value}).

%% error log
error(Value) ->
	gen_event:notify(?MODULE, {error, Value}).

%% Args for Handler:init/1
add_handler(Handler, Args) ->
	gen_event:add_handler(?MODULE, Handler, Args).

%% Args for Handler:terminate/2
delete_handler(Handler, Args) ->
	gen_event:delete_handler(?MODULE, Handler, Args).
