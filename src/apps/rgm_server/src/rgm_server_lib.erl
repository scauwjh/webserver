-module(rgm_server_lib).
-author("kei 2015-02-17").

-export([
	http_reply/1,
	http_reply/2,
	http_reply/3
]).

http_reply(Code, Headers, Body) ->
	ContentBytes = iolist_to_binary(Body),
	Length = byte_size(ContentBytes),
	RetHeader = io_lib:format("HTTP/1.1 ~s\r\n~sContent-Length: ~w\r\n\r\n",
		[response(Code), header(Headers), Length]),
	Reply = [RetHeader, ContentBytes],
	Reply.
http_reply(Code) ->
	http_reply(Code, <<>>).
http_reply(Code, Body) ->
	http_reply(Code, [{"Content-Type", "text/html"}], Body).


%%%========================================================================
%%% Internal functions
%%%========================================================================
header(Headers) ->
	header(Headers, []).

header([], RetHeader) ->
	RetHeader;
header([{Header, Text} | Rest], RetHeader) ->
	NewRetHeader = [io_lib:format("~s: ~s\r\n", [Header, Text]) | RetHeader],
	header(Rest, NewRetHeader).

%% response code mapping
response(100) -> "100 Continue";
response(200) -> "200 OK";
response(404) -> "404 Not Found";
response(501) -> "501 Not Implemented";
response(Code) -> integer_to_list(Code).