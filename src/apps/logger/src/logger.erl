-module(logger).
-author("kei 2015-03-05").
-description("Simple logger for web server").

-export([
	start_link/0,
	debug/1,
	info/1,
	error/1,
	debug/2,
	info/2,
	error/2,
	add_handler/2,
	delete_handler/2
]).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link() ->
	gen_event:start_link({local, ?MODULE}).

%% Args for Handler:init/1
add_handler(Handler, Args) ->
	gen_event:add_handler(?MODULE, Handler, Args).

%% Args for Handler:terminate/2
delete_handler(Handler, Args) ->
	gen_event:delete_handler(?MODULE, Handler, Args).


%% debug log
debug(Msg) ->
	gen_event:notify(?MODULE, {debug, Msg, []}).
%% debug log
debug(Msg, Value) ->
	gen_event:notify(?MODULE, {debug, Msg, Value}).

%% info log
info(Msg) ->
	gen_event:notify(?MODULE, {info, Msg, []}).
%% info log
info(Msg, Value) ->
	gen_event:notify(?MODULE, {info, Msg, Value}).

%% error log
error(Msg) ->
	gen_event:notify(?MODULE, {error, Msg, []}).
%% error log
error(Msg, Value) ->
	gen_event:notify(?MODULE, {error, Msg, Value}).
