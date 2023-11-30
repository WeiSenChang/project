%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 19:10
%%%-------------------------------------------------------------------
-module(mod_server).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([
    start_link/4,
    sync_apply/2,
    sync_apply/3,
    sync_stop/1,
    async_apply/2,
    async_apply/3,
    async_stop/1,
    async_stop/2,
    i/1,
    get_callback_mod/0,
    put_callback_mod/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CALL_BACK_MOD, callback_mod).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ProcessName, Mod, Args, Options) ->
    gen_server:start_link(ProcessName, ?MODULE, [Mod, Args], Options).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([Mod, Args]) ->
    try
        put_callback_mod(Mod),
        cast(self(), db_init),
        Mod:init(Args)
    catch
        _:Reason ->
            timer:sleep(1000),
            {error, Reason}
    end.

%% @private
%% @doc Handling call messages
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        _:_Reason ->
            ?WARING("~w", [_Reason]),
            {reply, ok, State}
    end.

%% @private
%% @doc Handling cast messages
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch
        _:_Reason ->
            ?WARING("~w, ~w", [get_callback_mod(), _Reason]),
            {noreply, State}
    end.

%% @private
%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        _:_Reason ->
            ?WARING("~w", [_Reason]),
            {noreply, State}
    end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(Reason, State) ->
    Mod = get_callback_mod(),
    try
        Mod:terminate(Reason, State)
    catch
        _:_StopReason ->
            ?WARING("~w", [_StopReason]),
            skip
    end,
    {stop, Reason, State}.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    Mod = get_callback_mod(),
    Mod:code_change(Mod, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 同步调用接口
do_call({sync_apply, Fun}, _From, State) ->
    Reply = Fun(),
    {reply, Reply, State};
do_call({sync_apply, Fun, Args}, _From, State) ->
    Reply = erlang:apply(Fun, Args),
    {reply, Reply, State};

%% 同步获取进程的state
do_call(get_status, _From, State) ->
    {reply, State, State};

%% 同步停止进程
do_call(stop, _From, State) ->
    {stop, normal, ok, State};

do_call(Request, From, State) ->
    Mod = get_callback_mod(),
    Mod:handle_call(Request, From, State).

%% 异步调用接口
do_cast({async_apply, Fun}, State) ->
    Fun(),
    {noreply, State};
do_cast({async_apply, Fun, Args}, State) ->
    erlang:apply(Fun, Args),
    {noreply, State};

%% 异步停止进程
do_cast(stop, State) ->
    {stop, normal, State};
do_cast({stop, Reason}, State) ->
    {stop, Reason, State};

do_cast(db_init, State) ->
    Mod = get_callback_mod(),
    Mod:db_init(State);

do_cast(Request, State) ->
    Mod = get_callback_mod(),
    Mod:handle_cast(Request, State).

%% 处理停止进程的消息stop
do_info(stop, State) ->
    {stop, normal, State};

do_info(Info, State) ->
    Mod = get_callback_mod(),
    Mod:handle_info(Info, State).

%% 同步发送消息
call(Pid, Request) ->
    gen_server:call(Pid, Request).

sync_apply(Pid, Fun) ->
    call(Pid, {sync_apply, Fun}).

sync_apply(Pid, Fun, Args) ->
    call(Pid, {sync_apply, Fun, Args}).

%% 同步停止进程
sync_stop(Pid) ->
    call(Pid, stop).

%% 异步发送消息
cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

async_apply(Pid, Fun) ->
    cast(Pid, {async_apply, Fun}).

async_apply(Pid, Fun, Args) ->
    cast(Pid, {async_apply, Fun, Args}).

async_stop(Pid)->
    cast(Pid, stop).
async_stop(Pid, Reason) ->
    cast(Pid, {stop, Reason}).

i(Pid) ->
    call(Pid, get_status).

put_callback_mod(Mod) ->
    erlang:put(?CALL_BACK_MOD, Mod).

get_callback_mod() ->
    erlang:get(?CALL_BACK_MOD).