-module(rgm_server_worker).
-author("kei 2015-02-16").
-description("rgm server").

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").

-export([
    start_link/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    lsock,
    socket,
    request_line,
    headers = [],
    body = <<>>,
    content_remaining = 0,
    callback,
    user_data,
    parent,
    keep_alive,
    last_modified
}).

%%%========================================================================
%%% External functions
%%%========================================================================
start_link(Callback, LSock, UserArgs) ->
    ?PRINT(?MODULE, "a new worker is started~n", []),
    gen_server:start_link(?MODULE, [Callback, LSock, UserArgs, self()], []).

init([Callback, LSock, UserArgs, Parent]) ->
    {ok, UserData} = Callback:init(UserArgs),
    State = #state{
        lsock = LSock,
        callback = Callback,
        user_data = UserData,
        parent = Parent
    },
    {ok, State, 0}. %% 延迟初始化到 handle_info(timeout, State)

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 解析请求行
handle_info({http, _Sock, {http_request, _, _, _} = Request}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, State#state{request_line = Request}};
%% 接收解析头部
handle_info({http, _Sock, {http_header, _, Name, _, Value}}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, header(Name, Value, State)};
%% remaining为0，数据接收完毕
handle_info({http, _Sock, http_eoh}, #state{content_remaining = 0} = State) ->
    case State#state.keep_alive of
    true ->
        %% 持久连接，不断开连接，初始化headers
        inet:setopts(State#state.socket, [{active, once}]),
        NewState = handle_http_request(State),
        {noreply, NewState#state{headers = []}};
    _Other ->
        %% 非持久连接，直接断开连接，销毁进程
        {stop, normal, handle_http_request(State)}
    end;
%% 数据接收
handle_info({http, _Sock, http_eoh}, State) ->
    inet:setopts(State#state.socket, [{active, once}, {packet, raw}]),
    {noreply, State};
%% tcp 数据接收处理
handle_info({tcp, _Sock, Data}, State) when is_binary(Data) ->
    ContentRem = State#state.content_remaining - byte_size(Data),
    Body = list_to_binary([State#state.body, Data]),
    NewState = State#state{body = Body, content_remaining = ContentRem},
    case ContentRem > 0 of
    true ->
        inet:setopts(State#state.socket, [{active, once}]),
        {noreply, NewState};
    _Other ->
        {stop, normal, handle_http_request(NewState)}
    end;
%% tcp连接关闭
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
%% 延迟初始化
%% gen_tcp:accept阻塞等待，收到信息向父进程(即当前gen_server进程)发异步消息
%% 并将当前进程挂接到rgm_connection_sup监督者下
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    rgm_connection_sup:start_child(Parent),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%% Content-Length
header('Content-Length' = Name, Value, State) ->
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength,
                headers = [{Name, Value} | State#state.headers]};
%% if keep-alive or not
header('Connection' = Name, <<"keep-alive">> = Value, State) ->
    State#state{keep_alive = true,
                headers = [{Name, Value} | State#state.headers]};
%% if modified since
header('If-Modified-Since', Value, State) ->
    State#state{last_modified = Value};
%% if modified since
header('Cache-Control' = Name, _Value, State) ->
    State#state{headers = [{Name, "public, max-age=15"} | State#state.headers]};

%% if keep-alive or not
%% header('Keep-Alive' = _Name, Value, State) ->
%%     io:format("keep-alive value=~p~n", [Value]),
%%     State;

%% Expect, reply code
header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
    gen_tcp:send(State#state.socket, rgm_server_lib:http_reply(100)),
    State#state{headers = [{Name, Value} | State#state.headers]};
%% Other header
header(Name, Value, State) ->
    State#state{headers = [{Name, Value} | State#state.headers]}.

%% handle the request
handle_http_request(#state{callback = Callback,
                        request_line = Request,
                        body = Body,
                        headers = Headers,
                        user_data = UserData,
                        last_modified = LastModified} = State) ->
    {http_request, Method, _, _} = Request,
    %% add other data, like lastmodifyed(If-Modified-Since)
    OtherData = #other_data{
        last_modified = LastModified,
        user_data = UserData
    },
    Reply = dispatch(Method, Request, Headers, Body, Callback, OtherData),
    gen_tcp:send(State#state.socket, Reply),
    State.

%% dispatchs
dispatch('GET', Request, Headers, _Body, Callback, OtherData) ->
    Callback:get(Request, Headers, OtherData);
dispatch('POST', Request, Headers, Body, Callback, OtherData) ->
    Callback:post(Request, Headers, Body, OtherData);
dispatch('DELETE', Request, Headers, _Body, Callback, OtherData) ->
    Callback:delete(Request, Headers, OtherData);
dispatch('HEAD', Request, Headers, _Body, Callback, OtherData) ->
    Callback:head(Request, Headers, OtherData);
dispatch('PUT', Request, Headers, Body, Callback, OtherData) ->
    Callback:put(Request, Headers, Body, OtherData);
dispatch('TRACE', Request, Headers, Body, Callback, OtherData) ->
    Callback:trace(Request, Headers, Body, OtherData);
dispatch('OPTIONS', Request, Headers, Body, Callback, OtherData) ->
    Callback:options(Request, Headers, Body, OtherData);
dispatch(_Other, Request, Headers, Body, Callback, OtherData) ->
    Callback:other_method(Request, Headers, Body, OtherData).
