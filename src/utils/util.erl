-module(util).
-author("kei 2015-02-15").
-description("util functions").

-include("common.hrl").

-export([
	random/1
]).

random(MaxRange) ->
	random:seed(erlang:now()),
	Num = trunc(random:uniform() * MaxRange),
	Num.