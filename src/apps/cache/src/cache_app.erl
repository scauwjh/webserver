-module(cache_app).
-author("kei 2015-02-28").

-behaviour(application).

-include("cache.hrl").

-export([
    start/2,
    stop/1
]).

%%%========================================================================
%%% External functions
%%%========================================================================
%% API
start(_StartType, _StartArgs) ->
    io:format("starting cache...~n"),
    %% contact other nodes
    ok = ensure_contact(),
    resource_discovery:add_local_resource(?RESOURCES_NODES, node()),
    resource_discovery:add_target_resource_type(?RESOURCES_NODES),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_RESOURCES),
    %% init mnesia and resource_discovery
    cache_store:init(),
    %% start supervisor
    case cache_sup:start_link() of
    {ok, Pid} ->
        {ok, Pid};
    Other ->
        logger:error("start cahce failed!", Other),
        {error, Other}
    end.

stop(_State) ->
    ok.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%% contact the nodes
ensure_contact() ->
    %% default nodes for the first contact
    Node1 = list_to_atom(lists:concat([?DEFAULT_CONTACT_NODE, '1@localhost'])),
    Node2 = list_to_atom(lists:concat([?DEFAULT_CONTACT_NODE, '2@localhost'])),
    DefaultNodes = [Node1, Node2],
    case get_env(cache, contact_nodes, DefaultNodes) of
    [] ->
        {error, no_contact_nodes};
    ContactNodes ->
        ensure_contact(ContactNodes)
    end.
%% contact the nodes
%% call net_adm:ping/1 to contact
ensure_contact(ContactNodes) ->
    Answering = [Node || Node <- ContactNodes, net_adm:ping(Node) =:= pong],
    case Answering of
    [] ->
        {error, no_contact_nodes_reachable};
    _ ->
        DefaultTime = ?DEFAULT_WAITE_FOR_NODE,
        WaitTime = get_env(cache, wait_time, DefaultTime),
        wait_for_nodes(length(Answering), WaitTime)
    end.

%% wait for contact
wait_for_nodes(MinNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
    case length(nodes()) > MinNodes of
    true ->
        ok;
    false ->
        timer:sleep(SliceTime),
        wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.

%% package the gen_env function
get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
    undefined ->
        Default;
    {ok, Value} ->
        Value
    end.