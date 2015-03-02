-module(cache).
-author("kei 2015-02-27").
-description("Simple cache for web server power by mnesia").

-export([
	insert/2,
	lookup/1,
	delete/1
]).

%% insert a k-v to cache
insert(Key, Value) ->
	case cache_store:lookup(Key) of
	{ok, Pid} ->
		%% replace the value
		cache_element:replace(Pid, Value);
	{error, _} ->
		%% does not exist, create a new element
		{ok, Pid} = cache_element:create_element(Value),
		cache_store:insert(Key, Pid)
	end.

lookup(Key) ->
	try
		%% lookup the pid then fetch the value
		{ok, Pid} = cache_store:lookup(Key),
		{ok, Value} = cache_element:fetch(Pid),
		{ok, Value}
	catch
		_Class:_Exception ->
			%% error, need to add log?
			{error, not_found}
	end.

delete(Key) ->
	case cache_store:lookup(Key) of
	{ok, Pid} ->
		cache_element:delete(Pid);
	{error, _Reason} ->
		ok
	end.