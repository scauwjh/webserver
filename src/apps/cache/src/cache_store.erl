-module(cache_store).
-author("kei 2015-02-27").
-description("Use mnesia to store the pid").

-include("cache.hrl").

%% API
-export([
    init/0,
    insert/2,
    delete/1,
    lookup/1
]).

-record(key_to_pid, {
    key,
    pid
}).

%%%========================================================================
%%% External functions
%%%========================================================================
init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, CacheNodes} = resource_discovery:fetch_resources(?RESOURCES_NODES),
    dynamic_db_init(lists:delete(node(), CacheNodes)).

insert(Key, Pid) ->
    mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

lookup(Key) ->
    case mnesia:dirty_read(key_to_pid, Key) of
    [{key_to_pid, Key, Pid}] ->
        case is_pid_alive(Pid) of
        true ->
            {ok, Pid};
        false ->
            {error, not_found}
        end;
    [] ->
        {error, not_found}
    end.

delete(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
    [#key_to_pid{} = Record] ->
        mnesia:dirty_delete_object(Record);
    _ ->
        ok
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%% this function will call at the time of the first node init
%% to create the table, call one times
dynamic_db_init([]) ->
    mnesia:create_table(key_to_pid, [
        {index, [pid]},
        {attributes, record_info(fields, key_to_pid)}
    ]);
dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).

add_extra_nodes([Node | T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, [Node]} ->
        mnesia:add_table_copy(key_to_pid, node(), ram_copies),
        Tables = mnesia:system_info(tables),
        mnesia:wait_for_table(Tables, ?WAIT_FOR_TABLES);
    _ ->
        add_extra_nodes(T)
    end.

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
    false ->
        false;
    true ->
        case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
        true ->
            true;
        false ->
            false;
        {badrpc, _Reason} ->
            false
        end
    end.