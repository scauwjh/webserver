-ifndef(COMMON_H).
-define(COMMON_H, true).

%% define debug print
%% add/delete {d, DEBUG} in Emakefile for debug print
%% -ifdef(debug).
-define(PRINT(Module, Msg, ValueList), io:format(
	lists:concat(["[", Module, " ~ts] ", Msg]), [util:format_date() | ValueList])
).
%% -else.
%% -define(PRINT(Module, Msg, ValueList), ok).
%% -endif.

-endif.