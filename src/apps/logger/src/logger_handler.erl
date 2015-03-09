-module(logger_handler).
-author("kei 2015-03-05").

-behaviour(gen_event).

%% gen_event callbacks
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	add_handler/0,
	delete_handler/0
]).

%%%========================================================================
%%% External functions
%%%========================================================================
%% API
add_handler() ->
    logger:add_handler(?MODULE, []).

delete_handler() ->
    logger:delete_handler(?MODULE, []).

%% gen_event callbacks
init([]) ->
	{ok, []}.


handle_event({error, Msg, Value}, State) ->
	io:format("[Error ~ts] Msg=~p. Info=~p.~n", [util:format_date(),
		Msg, Value]),
	{ok, State};
handle_event({info, Msg, Value}, State) ->
	io:format("[Info ~p] Msg=~p. Info=~p.~n", [util:format_date(),
		Msg, Value]),
	{ok, State};
handle_event({debug, Msg, Value}, State) ->
	io:format("[Debug ~w] Msg=~p. Info=~p.~n", [util:format_date(),
		Msg, Value]),
	{ok, State};
handle_event(Request, State) ->
	io:format("Request=~p~n", [Request]),
	{ok, State}.

handle_call(Request, State) ->
	io:format("call Request=~p~n", [Request]),
	{ok, ok, State}.

handle_info(Info, State) ->
	io:format("Info =~p~n", [Info]),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
