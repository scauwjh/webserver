-module(cache_element).
-author("kei 2015-02-27").
-description("Cache element, one element(gen_server process) for one data").

-behaviour(gen_server).

-include("cache.hrl").

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
	start_link/2,
	create_element/1,
	create_element/2,
	fetch/1,
	replace/2,
	delete/1
]).

-record(state, {
	value,
	lease_time, %% 淘汰时间
	start_time %% 起始时间
}).

%%%========================================================================
%%% External functions
%%%========================================================================
%% API
start_link(Value, LeaseTime) ->
	gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% create a new element
create_element(Value, LeaseTime) ->
	cache_sup:start_child(Value, LeaseTime).
create_element(Value) ->
	create_element(Value, ?DEFAULT_LEASE_TIME).

%% fetch the value
fetch(Pid) ->
	gen_server:call(Pid, fetch).

%% replace the value
replace(Pid, Value) ->
	gen_server:cast(Pid, {replace, Value}).

%% delete the element
delete(Pid) ->
	gen_server:cast(Pid, delete).


%% gen_server callbacks
%% init the data and time
init([Value, LeaseTime]) ->
	Now = calendar:local_time(),
	StartTime = calendar:datetime_to_gregorian_seconds(Now),
	{ok, #state{value = Value,
			lease_time = LeaseTime,
			start_time = StartTime},
		time_left(StartTime, LeaseTime)}.

%% fetch the value save in this element
handle_call(fetch, _From,  State) ->
	#state{value = Value,
		lease_time = LeaseTime,
		start_time = StartTime
	} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{reply, {ok, Value}, State, TimeLeft}.

%% replace the value save in this element
handle_cast({replace, Value}, State) ->
	#state{lease_time = LeaseTime,
		start_time = StartTime
	} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{noreply, State#state{value = Value}, TimeLeft};

%% delete the value save in this element
%% (to stop this process)
handle_cast(delete, State) ->
	{stop, normal, State}.

%% timeout to stop this process
handle_info(timeout, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	cache_store:delete(self()),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%========================================================================
%%% Internal functions
%%%========================================================================
time_left(_StartTime, infinity) ->
	infinity;
time_left(StartTime, LeaseTime) ->
	Now = calendar:local_time(),
	CurrentTime =  calendar:datetime_to_gregorian_seconds(Now),
	TimeElapsed = CurrentTime - StartTime,
	case LeaseTime - TimeElapsed of
		Time when Time =< 0 -> 0;
		Time -> Time * 1000
	end.