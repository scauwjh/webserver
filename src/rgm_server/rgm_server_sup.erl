-module(rgm_server_sup).
-author("kei 2015-02-17").

-behaviour(supervisor).

%% API
-export([
    start_link/3,
    start_link/4
]).

%% Supervisor callbacks
-export([
    init/1
]).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link(Callback, Port, UserArgs) ->
    start_link(Callback, undefined, Port, UserArgs).

start_link(Callback, IP, Port, UserArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
        [Callback, IP, Port, UserArgs]).

init([Callback, IP, Port, UserArgs]) ->
    Connection = {
        rgm_connection_sup,
        {rgm_connection_sup, start_link, [Callback, IP, Port, UserArgs]},
        permanent, 2000, supervisor, [rgm_connection_sup]
    },
    % Acceptor =
    Children = [Connection],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.