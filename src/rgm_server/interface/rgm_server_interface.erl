-module(rgm_server_interface).
-author("kei 2015-02-17").

-behaviour(rgm_server_behaviour).

-include("resource_init.hrl").
-include("server.hrl").

%% API
-export([
    start_link/1,
    start_link/2
]).

%% rgm_server_behaviour callbacks
-export([
    init/1,
    get/3,
    delete/3,
    put/4,
    post/4,
    head/3,
    options/4,
    trace/4,
    other_methods/4
]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Port) ->
    rgm_server_sup:start_link(?MODULE, Port, []).

start_link(IP, Port) ->
    rgm_server_sup:start_link(?MODULE, IP, Port, []).

%%%===================================================================
%%% rgm_server_behaviour callbacks
%%%===================================================================
init([]) ->
    {ok, []}.

get({http_request, 'GET', {abs_path, <<"/",Key/bytes>>}, _},
        Headers, OtherData) ->
    %% resolve the OtherData
    ClientLastModified = OtherData#other_data.last_modified,
    %% need to add resource dir
    Resource = lists:concat([?RESOURCES_DIR, binary_to_list(Key)]),
    case cache:lookup(Resource) of
    {ok, {ContentType, LastModified, Value}} ->
        %% add some new header
        Headers1 = rgm_server_lib:add_header('LastModified', LastModified, Headers),
        %% Headers2 = rgm_server_lib:add_header('Expires', "Sun, 22 Mar 2015 23:53:00 GMT", Headers1),
        %% io:format("LastModified=~p~n", [{LastModified, ClientLastModified}]),
        case LastModified =:= ClientLastModified of
        true ->
            NewHeaders = [{"Content-Type", ContentType} | Headers1],
            rgm_server_lib:http_reply(304, NewHeaders);
        false ->
            NewHeaders = [{"Content-Type", ContentType} | Headers1],
            rgm_server_lib:http_reply(200, NewHeaders, Value)
        end;
    {error, not_found} ->
        NewHeaders = [{"Content-Type", rgm_server_lib:get_content_type("html")} | Headers],
        rgm_server_lib:http_reply(404, NewHeaders, "404 - File Not Found")
    end.

delete({http_request, 'DELETE', {abs_path, <<"/",_Key/bytes>>}, _},
       _Head, _UserData) ->
    rgm_server_lib:http_reply(200).

put({http_request, 'PUT', {abs_path, <<"/",_Key/bytes>>}, _},
    _Head, _Body, _UserData) ->
    rgm_server_lib:http_reply(200).

post(_Request, _Head, _Body, _UserData) ->
    rgm_server_lib:http_reply(501).

head(_Request, _Head, _UserData) ->
    rgm_server_lib:http_reply(501).

options(_Request, _Head, _Body, _UserData) ->
    rgm_server_lib:http_reply(501).

trace(_Request, _Head, _Body, _UserData) ->
    rgm_server_lib:http_reply(501).

other_methods(_Request, _Head, _Body, _UserData) ->
    rgm_server_lib:http_reply(501).
