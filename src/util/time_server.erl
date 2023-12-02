%% -*- coding: utf-8 -*-

-module(time_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([get_pid/0, get_p_name/0]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MIN_MODS, []).
-define(HOUR_MODS, []).
-define(ZERO_MODS, []).

-define(MIN, min).
-define(HOUR, hour).
-define(ZERO, zero).

-record(time_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid() ->
    erlang:whereis(?MODULE).

get_p_name() ->
    ?MODULE.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTING),
    {ok, #time_server_state{}}.

handle_call(_Request, _From, State = #time_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #time_server_state{}) ->
    %% do db_init
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    set_timer(?MIN),
    set_timer(?HOUR),
    set_timer(?ZERO),
    restart_timer(),
    {noreply, State};
handle_cast(restart_timer, State = #time_server_state{}) ->
    restart_timer(),
    {noreply, State};
handle_cast(_Request, State = #time_server_state{}) ->
    {noreply, State}.

handle_info(?MIN, State = #time_server_state{}) ->
    Secs = set_timer(?MIN),
    ?INFO("next min ~w s", [Secs]),
    trigger_timer(?MIN, ?MIN_MODS),
    {noreply, State};
handle_info(?HOUR, State = #time_server_state{}) ->
    Secs = set_timer(?HOUR),
    ?INFO("next hour ~w s", [Secs]),
    trigger_timer(?HOUR, ?HOUR_MODS),
    {noreply, State};
handle_info(?ZERO, State = #time_server_state{}) ->
    Secs = set_timer(?ZERO),
    ?INFO("next zero ~w s", [Secs]),
    trigger_timer(?ZERO, ?ZERO_MODS),
    {noreply, State};
handle_info(_Info, State = #time_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #time_server_state{}) ->
    ok.

code_change(_OldVsn, State = #time_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_timer(?MIN) ->
    set_timer(?MIN, lib_time:next_min_time());
set_timer(?HOUR) ->
    set_timer(?HOUR, lib_time:next_hour_time());
set_timer(?ZERO) ->
    set_timer(?ZERO, lib_time:next_zero_time()).

restart_timer() ->
    erlang:cancel_timer(get(?MIN)),
    erlang:cancel_timer(get(?HOUR)),
    erlang:cancel_timer(get(?ZERO)),
    NextMinSecs = set_timer(?MIN),
    NextHourSecs = set_timer(?HOUR),
    NextZeroSecs = set_timer(?ZERO),
    ?INFO("next min ~w s", [NextMinSecs]),
    ?INFO("next hour ~w s", [NextHourSecs]),
    ?INFO("next zero ~w s", [NextZeroSecs]).

trigger_timer(_Fun, []) ->
    ok;
trigger_timer(Fun, [Mod | Tail]) ->
    Mod:Fun(),
    trigger_timer(Fun, Tail).

set_timer(Type, NextSecs) ->
    Ref = erlang:send_after(NextSecs * 1000, self(), Type),
    erlang:put(Type, Ref),
    NextSecs.