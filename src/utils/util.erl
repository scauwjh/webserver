-module(util).
-author("kei 2015-02-15").
-description("util functions").

-include("common.hrl").

-export([
	clear/0,
    random/1,
    format_date/0,
    is_end_of/2
]).

clear() ->
	io:format("~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n
		~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n").

random(MaxRange) ->
    random:seed(erlang:now()),
    trunc(random:uniform() * MaxRange).

%% return atom
format_date() ->
    {{Y,MON,D}, {H,MIN,S}} = calendar:local_time(),
    list_to_atom(lists:concat([Y, "-", MON, "-", D, " ", H, ":", MIN, ":", S])).

is_end_of(EndStr, String) when is_list(EndStr) ->
	EndLen = length(EndStr),
	StrLen = length(String),
	case StrLen >= EndLen andalso lists:split(StrLen - EndLen, String) of
	{_H, EndStr} ->
		true;
	_Other ->
		false
	end.