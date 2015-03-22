-module(rgm_server_lib).
-author("kei 2015-02-17").

-export([
    http_reply/1,
    http_reply/2,
    http_reply/3,
    add_header/3,
    get_content_type/1
]).

%%%========================================================================
%%% External functions
%%%========================================================================
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
    http_reply(Code, [{"Content-Type", get_content_type(default)}], Body).

%% to add new header
add_header(Name, Value, Headers) ->
    [{Name, Value} | Headers].

%% content type
get_content_type("html") -> 'text/html';
get_content_type("htm") -> 'text/html';
get_content_type("jpg") -> 'image/jpeg';
get_content_type("png") -> 'image/png';
get_content_type("gif") -> 'image/gif';
get_content_type("ico") -> 'image/x-icon';
get_content_type("jpe") -> 'image/jpeg';
get_content_type("jpeg") -> 'image/jpeg';
get_content_type("js") -> 'application/x-javascript';
get_content_type("xml") -> 'text/xml';
get_content_type("xls") -> 'text/xml';
get_content_type("css") -> 'text/css';
get_content_type("txt") -> 'text/plain';
get_content_type(_) -> 'text/plain'.

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
response(304) -> "304 Not Modified";
response(404) -> "404 Not Found";
response(501) -> "501 Not Implemented";
response(Code) -> integer_to_list(Code).