%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 5月 2023 21:58
%%%-------------------------------------------------------------------
-module(mod_log).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, msg/4, async_msg/1, get_pid/0, hour/0, async_hour/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(mod_log_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mod_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_pid() ->
    erlang:whereis(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_log_state{}} | {ok, State :: #mod_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    case open_log() of
        {ok, Fd} ->
            set_log_fd(Fd);
        _ ->
            skip
    end,
    {ok, #mod_log_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_log_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_log_state{}} |
    {reply, Reply :: term(), NewState :: #mod_log_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_log_state{}} |
    {noreply, NewState :: #mod_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_log_state{}} |
    {stop, Reason :: term(), NewState :: #mod_log_state{}}).
handle_call(_Request, _From, State = #mod_log_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_log_state{}) ->
    {noreply, NewState :: #mod_log_state{}} |
    {noreply, NewState :: #mod_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_log_state{}}).
handle_cast(_Request, State = #mod_log_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_log_state{}) ->
    {noreply, NewState :: #mod_log_state{}} |
    {noreply, NewState :: #mod_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_log_state{}}).
handle_info(_Info, State = #mod_log_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_log_state{}) -> term()).
terminate(_Reason, _State = #mod_log_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_log_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_log_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_log_state{}, _Extra) ->
    mod_server:put_callback_mod(?MODULE),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
msg(Mod, Line, Format, Args) ->
    NewFormat = "[~.2.0w:~.2.0w:~.2.0w][~w][~w:~w] " ++ Format ++ "~n",
    {_Date, {H, M, S}} = lib_timer:to_local_time(lib_timer:unix_time()),
    NewArgs = [H, M, S, self(), Mod, Line | Args],
    Log = io_lib:format(NewFormat, NewArgs),
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_msg/1, [Log]).

async_msg(Log) ->
    case get_log_fd() of
        undefined ->
            case open_log() of
                {ok, NewFd} ->
                    set_log_fd(NewFd),
                    io:format(NewFd, "~ts", [Log]);
                _ ->
                    skip
            end;
        Fd ->
            io:format(Fd, "~ts", [Log])
    end.

open_log() ->
    File = lib_common:log_tab("log"),
    Fn = "./log/" ++ File ++ ".log",
    filelib:ensure_dir(Fn),
    file:open(Fn, [append]).


hour() ->
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_hour/0).
async_hour() ->
    case get_log_fd() of
        undefined -> skip;
        Fd ->
            file:close(Fd),
            file:sync(Fd)
    end,
    case open_log() of
        {ok, NewFd} ->
            set_log_fd(NewFd);
        _ ->
            skip
    end.

set_log_fd(Fd) ->
    erlang:put(log_fd, Fd).

get_log_fd() ->
    erlang:get(log_fd).