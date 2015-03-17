-module(util).
-author("kei 2015-02-15").
-description("util functions").

-include("common.hrl").

-export([
    random/1,
    format_date/0
]).

random(MaxRange) ->
    random:seed(erlang:now()),
    trunc(random:uniform() * MaxRange).

%% return atom
format_date() ->
    {{Y,MON,D}, {H,MIN,S}} = calendar:local_time(),
    list_to_atom(lists:concat([Y, "-", MON, "-", D, " ", H, ":", MIN, ":", S])).