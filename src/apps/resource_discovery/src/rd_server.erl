-module(rd_server).
-author("kei 2015-02-28").
-description("Resource discovery app server").

-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API
-export([
    start_link/0,
    add_target_resource_type/1,
    add_local_resource/2,
    fetch_resources/1,
    trade_resources/0
]).

-record(state, {
    target_resource_types, %% target resource types list
    local_resources, %% local resources dict
    found_resources %% found resources dict, fetch resources from here
}).

%%%========================================================================
%%% External functions
%%%========================================================================
%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_target_resource_type(Type) ->
    gen_server:cast(?MODULE, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?MODULE, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?MODULE, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?MODULE, trade_resources).

%% gen_server callbacks
init([]) ->
    {ok, #state{target_resource_types = [],
                local_resources = dict:new(),
                found_resources = dict:new()}}.

%% fetch the found resources
%% this sources would be added when trade_resources
handle_call({fetch_resources, Type}, _From, State) ->
    FoundResources = dict:find(Type, State#state.found_resources),
    {reply, FoundResources, State}.

%% add a new resource type
handle_cast({add_target_resource_type, Type}, State) ->
    OldTypes = State#state.target_resource_types,
    NewTypes = [Type | lists:delete(Type, OldTypes)],
    {noreply, State#state{target_resource_types = NewTypes}};
%% add local resource
handle_cast({add_local_resource, {Type, Resource}}, State) ->
    OldResourcesDict = State#state.local_resources,
    NewResourcesDict = add_resource(Type, Resource, OldResourcesDict),
    {noreply, State#state{local_resources = NewResourcesDict}};
%% trade resources
handle_cast(trade_resources, State) ->
    ResourcesDict = State#state.local_resources,
    AllNodes = [node() | nodes()],
    F = fun(Node) ->
        gen_server:cast({?MODULE, Node},
            {trade_resources, {node(), ResourcesDict}})
    end,
    lists:foreach(F, AllNodes),
    {noreply, State};
%% trade resource with remote nodes
%% ReplyTo is the node of the caster, reply to this node
handle_cast({trade_resources, {ReplyTo, RemoteResourcesDict}},
        #state{
            local_resources = LocalResources,
            target_resource_types = TargetTypes,
            found_resources = OldFoundResources
        } = State) ->
    F = fun(Type, Acc) ->
        case dict:find(Type, RemoteResourcesDict) of
        {ok, List} ->
            [{Type, Resource} || Resource <- List] ++ Acc;
        error ->
            Acc
        end
    end,
    AddResourcesList = lists:foldl(F, [], TargetTypes),
    NewFoundResources = add_resources(AddResourcesList, OldFoundResources),
    %% reply local resources to caster
    reply_trade(ReplyTo, LocalResources),
    {noreply, State#state{found_resources = NewFoundResources}}.

handle_info(ok = _Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%% reply local resources to caster
reply_trade(ReplyTo, LocalResources) ->
    case ReplyTo of
    noreply ->
        ok;
    _Other ->
        gen_server:cast({?MODULE, ReplyTo},
            {trade_resources, {noreply, LocalResources}})
    end.

%% add resources to dict
add_resources([{Type, Resource}|T], Dict) ->
    add_resources(T, add_resource(Type, Resource, Dict));
add_resources([], Dict) ->
    Dict.

add_resource(Type, Resource, OldResourcesDict) ->
    case dict:find(Type, OldResourcesDict) of
    {ok, OldResources} ->
        NewResources = [Resource | lists:delete(Resource, OldResources)],
        NewDict = dict:store(Type, NewResources, OldResourcesDict);
    error ->
        NewDict = dict:store(Type, [Resource], OldResourcesDict)
    end,
    NewDict.





