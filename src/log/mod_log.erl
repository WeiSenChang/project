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
-export([start_link/0, msg/3, msg/4, async_msg/1, get_pid/0,
    hour/0, async_hour/0, zero/0, async_zero/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3, db_init/2]).

-define(SERVER, ?MODULE).
-define(LOG_PATH, "./log/").
-define(LOG_FD, log_fd).
-define(LOG_LIST, log_list).

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
    erlang:send_after(10 * 1000, self(), write),
    {ok, #mod_log_state{}}.

db_init(#mod_log_state{} = State, _Args) ->
    open_log(),
    {noreply, State}.

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
handle_info(write, State = #mod_log_state{}) ->
    erlang:send_after(10 * 1000, self(), write),
    write_log(),
    {noreply, State};
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
    write_log(),
    close_log(),
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_log_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_log_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_log_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
msg(Mod, Line, Format) ->
    msg(Mod, Line, Format, []).
msg(Mod, Line, Format, Args) ->
    NewFormat = "[~.2.0w:~.2.0w:~.2.0w][~w][~w:~w] " ++ Format ++ "~n",
    {_Date, {H, M, S}} = lib_timer:to_local_time(lib_timer:unix_time()),
    NewArgs = [H, M, S, self(), Mod, Line | Args],
    Log = io_lib:format(NewFormat, NewArgs),
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_msg/1, [Log]).

async_msg(Log) ->
    io:format(Log),
    LogList = get_log_list(),
    put_log_list([{lib_timer:unix_time(), Log}|LogList]),
    ok.

hour() ->
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_hour/0, []).

async_hour() ->
    ?DEBUG("log hour"),
    LogList = get_log_list(),
    NowTick = lib_timer:unix_time(),
    {LogList1, LogList2} = lists:splitwith(fun({Tick, _}) -> Tick < NowTick end, LogList),
    put_log_list(LogList2),
    case get_fd() of
        undefined -> ok;
        Fd ->
            write_log(Fd, lists:reverse(LogList1)),
            file:sync(Fd)
    end,
    close_log(),
    open_log(),
    ok.

zero() ->
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_zero/0, []).

async_zero() ->
    ?DEBUG("log zero"),
    ok.

%%%%%%
put_fd(Fd) ->
    erlang:put(?LOG_FD, Fd).

get_fd() ->
    erlang:get(?LOG_FD).

get_log_list() ->
    case erlang:get(?LOG_LIST) of
        undefined -> [];
        LogList -> LogList
    end.

put_log_list(LogList) ->
    erlang:put(?LOG_LIST, LogList).

close_log() ->
    case get_fd() of
        undefined -> ok;
        Fd ->
            file:close(Fd),
            file:sync(Fd)
    end.

open_log() ->
    Mod = lib_common:log_tab("log"),
    File = erlang:atom_to_list(Mod),
    FileName = ?LOG_PATH ++ File ++ ".log",
    filelib:ensure_dir(FileName),
    case file:open(FileName, [append]) of
        {ok, Fd} ->
            file:sync(Fd),
            put_fd(Fd),
            ok;
        _ -> fail
    end.

write_log() ->
    LogList = get_log_list(),
    put_log_list([]),
    case get_fd() of
        undefined -> ok;
        Fd ->
            write_log(Fd, lists:reverse(LogList)),
            file:sync(Fd)
    end.

write_log(_Fd, []) ->
    ok;
write_log(Fd, [{_, Log}|T]) ->
    io:format(Fd, "~ts", [Log]),
    write_log(Fd, T).