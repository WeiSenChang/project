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
-export([start_link/0, get_pid/0, pass_secs/0, get_secs/0, set_secs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MIN_NTF_MODS, []).
-define(HOUR_NTF_MODS, [mod_log]).
-define(ZERO_NTF_MODS, []).
-define(PASS_SECS, pass_secs).

-record(mod_timer_state, {min_ref, hour_ref, zero_ref}).

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
    NewState = restart_timer(#mod_timer_state{}),
    {ok, NewState}.

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

handle_info(min, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_min_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), min),
    ?INFO("next min: ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:min() end, ?MIN_NTF_MODS),
    {noreply, State#mod_timer_state{min_ref = NewRef}};
handle_info(hour, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_hour_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), hour),
    ?INFO("next hour ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:hour() end, ?HOUR_NTF_MODS),
    {noreply, State#mod_timer_state{hour_ref = NewRef}};
handle_info(zero, State = #mod_timer_state{}) ->
    Secs = lib_timer:next_zero_time(),
    NewRef = erlang:send_after(Secs * 1000, self(), zero),
    ?INFO("next zero ~w s", [Secs]),
    lists:foreach(fun(Mod) -> Mod:zero() end, ?ZERO_NTF_MODS),
    {noreply, State#mod_timer_state{zero_ref = NewRef}};
handle_info(restart, State = #mod_timer_state{}) ->
    ?INFO("restart timer"),
    NewState = restart_timer(State),
    {noreply, NewState};
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
restart_timer(State) ->
    #mod_timer_state{min_ref = MinRef, hour_ref = HourRef, zero_ref = ZeroRef} = State,
    check_cancel_timer(MinRef),
    check_cancel_timer(HourRef),
    check_cancel_timer(ZeroRef),
    NextMinSecs = lib_timer:next_min_time(),
    NextHourSecs = lib_timer:next_hour_time(),
    NextZeroSecs = lib_timer:next_zero_time(),
    NewMinRef = erlang:send_after(NextMinSecs * 1000, self(), min),
    NewHourRef = erlang:send_after(NextHourSecs * 1000, self(), hour),
    NewZeroRef = erlang:send_after(NextZeroSecs * 1000, self(), zero),
    ?INFO("next min ~w s", [NextMinSecs]),
    ?INFO("next hour ~w s", [NextHourSecs]),
    ?INFO("next zero ~w s", [NextZeroSecs]),
    State#mod_timer_state{min_ref = NewMinRef, hour_ref = NewHourRef, zero_ref = NewZeroRef}.

check_cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
check_cancel_timer(_Other) ->
    skip.


pass_secs() ->
    mod_server:sync_apply(mod_timer:get_pid(), fun mod_timer:get_secs/0).

get_secs() ->
    case erlang:get(?PASS_SECS) of
        undefined -> 0;
        Secs -> Secs
    end.

set_secs(Sec) ->
    erlang:put(?PASS_SECS, Sec),
    self() ! restart.