%% -*- coding: utf-8 -*-

-module(log_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([get_pid/0, get_p_name/0, debug_msg/4, info_msg/4, waring_msg/4]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO, 1).
-define(LOG_LEVEL_WARING, 2).

-record(log_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid() ->
    erlang:whereis(?MODULE).

get_p_name() ->
    ?MODULE.

debug_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_DEBUG,
    msg(Mod, Line, Format, Args, LogLevel).

info_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_INFO,
    msg(Mod, Line, Format, Args, LogLevel).

waring_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_WARING,
    msg(Mod, Line, Format, Args, LogLevel).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTING),
    {ok, #log_server_state{}}.

handle_call(_Request, _From, State = #log_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #log_server_state{}) ->
    %% do db_init
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    {noreply, State};
handle_cast({msg, Log, LogLevel}, State = #log_server_state{}) ->
    msg(Log, LogLevel),
    {noreply, State};
handle_cast(_Request, State = #log_server_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #log_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #log_server_state{}) ->
    ok.

code_change(_OldVsn, State = #log_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
msg(Log, LogLevel) ->
    io:format(Log),
    case LogLevel >= ?LOG_LEVEL_DEBUG of
        true -> error_logger:info_msg(Log);
        false -> ignore
    end.

msg(Mod, Line, Format, Args, LogLevel) ->
    Log = gen_log(Mod, Line, Format, Args, LogLevel),
    gen_server:cast(get_pid(), {msg, Log, LogLevel}).

gen_log(Mod, Line, Format, Args, LogLevel) ->
    {{Y, Mon, D}, {H, Min, S}} = lib_time:to_local_time(lib_time:unix_time()),
    NewFormat = "[~.4.0w/~.2.0w/~.2.0w][~.2.0w:~.2.0w:~.2.0w][" ++
        gen_log_level(LogLevel) ++ "][~w][~w:~w] " ++ Format ++ "~n",
    NewArgs = [Y, Mon, D, H, Min, S, self(), Mod, Line | Args],
    io_lib:format(NewFormat, NewArgs).

gen_log_level(?LOG_LEVEL_DEBUG) ->
    "DEBUG";
gen_log_level(?LOG_LEVEL_INFO) ->
    "INFO";
gen_log_level(?LOG_LEVEL_WARING) ->
    "WARING".