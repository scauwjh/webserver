-module(resource_init).
-author("kei 2015-03-03").
-description("Init the resources when starting the server").

-behaviour(gen_server).

-include("common.hrl").
-include("resource_init.hrl").

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
    start_link/0
]).

%%%========================================================================
%%% External functions
%%%========================================================================
%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    ets:new(?ETS_RESOURCES, [set, protected, named_table, {keypos, 1}]),
    init_resource(),
    erlang:send_after(?INIT_SCAN_TIME,
        erlang:whereis(?MODULE), init_resources),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init_resources, State) ->
    %% init again
    init_resource(),
    case erlang:whereis(?MODULE) of
    Pid when is_pid(Pid) ->
        erlang:send_after(?INIT_SCAN_TIME, Pid, init_resources),
        {noreply, State};
    Other ->
        logger:error("Pid is not a pid", Other),
        {stop, normal, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%========================================================================
%%% Internal functions
%%%========================================================================
%% reinit the cache if file modified
init_resource() ->
    AllFileList = init_resource(?RESOURCES_DIR, []),
    update_resource_ets(AllFileList).

init_resource(Dir, AllFileList) ->
    %% io:format("Dir is=~p~n", [Dir]),
    %% timer:sleep(2000),
    {ok, FileNameList} = file:list_dir(Dir),
    init_resource(FileNameList, Dir, AllFileList).

init_resource([], _ParentDir, AllFileList) ->
    AllFileList;
init_resource([FileName | T], ParentDir, AllFileList) ->
    NewDir = fix_dir(ParentDir),
    %% add the head path
    File = lists:concat([NewDir, FileName]),
    case filelib:is_dir(File) of
    true->
        NewAllFileList = init_resource(File, AllFileList),
        init_resource(T, NewDir, NewAllFileList);
    false ->
        NewAllFileList = [File | AllFileList],
        insert_resource_ets(File),
        init_resource(T, NewDir, NewAllFileList)
    end.



%% when the resource is not exist, insert to the ets
insert_resource_ets(File) ->
    LastModified = filelib:last_modified(File),
    case ets:lookup(?ETS_RESOURCES, File) of
    [{File, LastModified}] ->
        skip;
    _other ->
        io:format("update resource: ~p~n", [File]),
        %% update ets
        ets:insert(?ETS_RESOURCES, {File, LastModified}),
        {ok, Bin} = file:read_file(File),
        %% get content-type by file suffix
        [Suffix | _Rest] = lists:reverse(
            re:split(File, "[.]", [{return,list}])
        ),
        ContentType = rgm_server_lib:get_content_type(Suffix),
        cache:insert(File, {ContentType, binary_to_list(Bin)})
    end.

%% when one or many of the resources is deleted, update the ets
update_resource_ets(FileList) ->
    ResourceList = ets:tab2list(?ETS_RESOURCES),
    %% io:format("FileList=~p~nResourceList=~p~n", [FileList, ResourceList]),
    F = fun({File, _}) ->
        case lists:member(File, FileList) of
        true ->
            skip;
        false ->
            case filelib:is_dir(File) of
            true ->
                skip;
            false ->
                io:format("delete resource: ~p~n", [File]),
                %% update ets
                ets:delete(?ETS_RESOURCES, File),
                %% update cache
                cache:delete(File)
            end
        end
    end,
    lists:foreach(F, ResourceList).

%% fix the file dir if not end with "/"
fix_dir(Dir) when is_list(Dir) ->
    case util:is_end_of("/", Dir) of
    true ->
        NewDir = Dir;
    false ->
        NewDir = lists:concat([Dir, "/"])
    end,
    NewDir;
fix_dir(Dir) when is_atom(Dir) ->
    NewDir = atom_to_list(Dir),
    fix_dir(NewDir).