%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 5月 2023 23:45
%%%-------------------------------------------------------------------
-module(mod_timer).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, get_pid/0, db_init/1, get_secs/0, set_secs/1, restart_timer/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MIN_NTF_MODS, []).
-define(HOUR_NTF_MODS, []).
-define(ZERO_NTF_MODS, []).

-define(MIN_REF, min_ref).
-define(HOUR_REF, hour_ref).
-define(ZERO_REF, zero_ref).
-define(ETS_PASS_SECS, ets_pass_secs).

-record(mod_timer_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mod_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_pid() ->
    erlang:whereis(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_timer_state{}} | {ok, State :: #mod_timer_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ets:new(?ETS_PASS_SECS, [named_table, public]),
    restart_timer(),
    lib_server:set_server_state(?SERVER, ?SERVER_STARTING),
    {ok, #mod_timer_state{}}.

db_init(State = #mod_timer_state{}) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTED),
    {noreply, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_timer_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_timer_state{}} |
    {reply, Reply :: term(), NewState :: #mod_timer_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_timer_state{}} |
    {noreply, NewState :: #mod_timer_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_timer_state{}} |
    {stop, Reason :: term(), NewState :: #mod_timer_state{}}).
handle_call(_Request, _From, State = #mod_timer_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_timer_state{}) ->
    {noreply, NewState :: #mod_timer_state{}} |
    {noreply, NewState :: #mod_timer_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_timer_state{}}).
handle_cast(_Request, State = #mod_timer_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_timer_state{}) ->
    {noreply, NewState :: #mod_timer_state{}} |
    {noreply, NewState :: #mod_timer_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_timer_state{}}).

handle_info(?MIN_REF, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_min_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), ?MIN_REF),
    set_ref(?MIN_REF, NewRef),
    ?INFO("next min ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:min() end, ?MIN_NTF_MODS),
    {noreply, State};
handle_info(?HOUR_REF, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_hour_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), ?HOUR_REF),
    set_ref(?HOUR_REF, NewRef),
    ?INFO("next hour ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:hour() end, ?HOUR_NTF_MODS),
    {noreply, State};
handle_info(?ZERO_REF, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_zero_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), ?ZERO_REF),
    set_ref(?ZERO_REF, NewRef),
    ?INFO("next zero ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:zero() end, ?ZERO_NTF_MODS),
    {noreply, State};
handle_info(_Info, State = #mod_timer_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_timer_state{}) -> term()).
terminate(_Reason, _State = #mod_timer_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_timer_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_timer_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_timer_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
restart_timer() ->
    MinRef = get_ref(?MIN_REF),
    HourRef = get_ref(?HOUR_REF),
    ZeroRef = get_ref(?ZERO_REF),
    check_cancel_timer(MinRef),
    check_cancel_timer(HourRef),
    check_cancel_timer(ZeroRef),
    NextMinSecs = lib_timer:next_min_time(),
    NextHourSecs = lib_timer:next_hour_time(),
    NextZeroSecs = lib_timer:next_zero_time(),
    NewMinRef = erlang:send_after(NextMinSecs * 1000, self(), ?MIN_REF),
    NewHourRef = erlang:send_after(NextHourSecs * 1000, self(), ?HOUR_REF),
    NewZeroRef = erlang:send_after(NextZeroSecs * 1000, self(), ?ZERO_REF),
    set_ref(?MIN_REF, NewMinRef),
    set_ref(?HOUR_REF, NewHourRef),
    set_ref(?ZERO_REF, NewZeroRef),
    ?INFO("next min ~w s", [NextMinSecs]),
    ?INFO("next hour ~w s", [NextHourSecs]),
    ?INFO("next zero ~w s", [NextZeroSecs]).

check_cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
check_cancel_timer(_Other) ->
    skip.

get_secs() ->
    case ets:lookup(?ETS_PASS_SECS, ?ETS_PASS_SECS) of
        [{?ETS_PASS_SECS, Secs}] -> Secs;
        _ -> 0
    end.

set_secs(Secs) ->
    ets:insert(?ETS_PASS_SECS, {?ETS_PASS_SECS, Secs}),
    mod_server:async_apply(mod_timer:get_pid(), fun mod_timer:restart_timer/0).

get_ref(Type) ->
    erlang:get(Type).

set_ref(Type, Ref) ->
    erlang:put(Type, Ref).