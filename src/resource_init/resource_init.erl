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
	init_resources(),
	erlang:send_after(?INIT_SCAN_TIME,
		erlang:whereis(?MODULE), init_resources),
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(init_resources, State) ->
	%% init again
	init_resources(),
	case erlang:whereis(?MODULE) of
	Pid when is_pid(Pid) ->
		erlang:send_after(?INIT_SCAN_TIME, Pid, init_resources),
		{noreply, State};
	_Other ->
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
%% init the static resources, in the default path
init_resources() ->
	{ok, FileNameList} = file:list_dir("../resources/"),
	update_resource_ets(FileNameList),
	init_resource(FileNameList).

%% reinit the cache if file modified
init_resource([FileName | T]) ->
	File = lists:concat(["../resources/", FileName]),
	LastModified = filelib:last_modified(File),
	case ets:lookup(?ETS_RESOURCES, FileName) of
	[{FileName, LastModified}] ->
		skip;
	_other ->
		io:format("update resource: ~p~n", [FileName]),
		%% update ets
		ets:insert(?ETS_RESOURCES, {FileName, LastModified}),
		{ok, Bin} = file:read_file(File),
		%% update cache
		cache:insert(FileName, binary_to_list(Bin))
	end,
	init_resource(T);
init_resource([]) ->
	ok.

%% when the resource is delete update the ets
update_resource_ets(FileNameList) ->
	ResourceList = ets:tab2list(?ETS_RESOURCES),
	F = fun({FileName, _}) ->
		case lists:member(FileName, FileNameList) of
		true ->
			skip;
		false ->
			io:format("delete resource: ~p~n", [FileName]),
			%% update ets
			ets:delete(?ETS_RESOURCES, FileName),
			%% update cache
			cache:delete(FileName)
		end
	end,
	lists:foreach(F, ResourceList).
