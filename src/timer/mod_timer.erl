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
-include("timer.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3, db_init/2]).

-define(SERVER, ?MODULE).

-record(mod_timer_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mod_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_timer_state{}} | {ok, State :: #mod_timer_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:send_after(lib_timer:next_min_time() * 1000, self(), min),
    erlang:send_after(lib_timer:next_hour_time() * 1000, self(), hour),
    erlang:send_after(lib_timer:next_zero_time() * 1000, self(), zero),
    {ok, #mod_timer_state{}}.

db_init(State = #mod_timer_state{}, _Args) ->
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

handle_info(min, State = #mod_timer_state{}) ->
    erlang:send_after(lib_timer:next_min_time() * 1000, self(), min),
    min(),
    {noreply, State};
handle_info(hour, State = #mod_timer_state{}) ->
    erlang:send_after(lib_timer:next_hour_time() * 1000, self(), hour),
    hour(),
    {noreply, State};
handle_info(zero, State = #mod_timer_state{}) ->
    erlang:send_after(lib_timer:next_zero_time() * 1000, self(), zero),
    zero(),
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
min() ->
    notify(?NOTIFY_MIN_MODS, min).

hour() ->
    erlang:garbage_collect(),
    notify(?NOTIFY_HOUR_MODS, hour).

zero() ->
    notify(?NOTIFY_ZERO_MODS, zero).

%%%%%%
notify([], _Name) ->
    ok;
notify([Mod|T], Name) ->
    Mod:Name(),
    notify(T, Name).