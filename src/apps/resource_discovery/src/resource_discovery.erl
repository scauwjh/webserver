-module(resource_discovery).
-author("kei 2015-02-28").
-description("Resource discovery app, for synchronizing the datas").

%% API
-export([
    add_target_resource_type/1,
    add_local_resource/2,
    fetch_resources/1,
    trade_resources/0
]).

add_target_resource_type(Type) ->
    rd_server:add_target_resource_type(Type).

add_local_resource(Type, Resource) ->
    rd_server:add_local_resource(Type, Resource).

fetch_resources(Type) ->
    rd_server:fetch_resources(Type).

trade_resources() ->
    rd_server:trade_resources().