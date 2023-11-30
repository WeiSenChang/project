%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(mod_log).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([debug_msg/4, info_msg/4, waring_msg/4, async_msg/2]).

-export([start_link/0, get_pid/0, db_init/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO, 1).
-define(LOG_LEVEL_WARING, 2).

-record(mod_log_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    mod_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_pid() ->
    erlang:whereis(?SERVER).

init([]) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTING),
    {ok, #mod_log_state{}}.

db_init(State) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTED),
    {noreply, State}.

handle_call(_Request, _From, State = #mod_log_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #mod_log_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #mod_log_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #mod_log_state{}) ->
    ok.

code_change(_OldVsn, State = #mod_log_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

debug_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_DEBUG,
    msg(Mod, Line, Format, Args, LogLevel).

info_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_INFO,
    msg(Mod, Line, Format, Args, LogLevel).

waring_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_WARING,
    msg(Mod, Line, Format, Args, LogLevel).


msg(Mod, Line, Format, Args, LogLevel) ->
    Log = gen_log(Mod, Line, Format, Args),
    mod_server:async_apply(mod_log:get_pid(), fun mod_log:async_msg/2, [Log, LogLevel]).

async_msg(Log, LogLevel) ->
    case LogLevel of
        ?LOG_LEVEL_DEBUG ->
            {{Y, Mon, D}, {H, Min, S}} = lib_timer:to_local_time(lib_timer:unix_time()),
            MonStr = lib_timer:month_to_str(Mon),
            HeadLog = io_lib:format("=DEBUG REPORT==== ~.2.0w-~ts-~.4.0w::~.2.0w:~.2.0w:~.2.0w ===~n", [D, MonStr, Y, H, Min, S]),
            io:format(HeadLog ++ Log);
        ?LOG_LEVEL_INFO ->
            error_logger:info_msg(Log);
        ?LOG_LEVEL_WARING ->
            error_logger:warning_msg(Log);
        _ ->
            error_logger:format(Log, [])
    end.

gen_log(Mod, Line, Format, Args) ->
    NewFormat = "[Pid:~w][Mod:~w][Line:~w][Msg:\"" ++ Format ++ "\"]~n",
    NewArgs = [self(), Mod, Line | Args],
    io_lib:format(NewFormat, NewArgs).