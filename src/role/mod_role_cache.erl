%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 8月 2023 19:05
%%%-------------------------------------------------------------------
-module(mod_role_cache).
-author("weisenchang").

-behaviour(gen_server).
-include("common.hrl").

%% API
-export([start_link/0, db_init/2, get_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mod_role_cache_state, {}).

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
    {ok, State :: #mod_role_cache_state{}} | {ok, State :: #mod_role_cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    lib_role_cache:ets_init(),
    erlang:send_after(60 * 1000, self(), save),
    {ok, #mod_role_cache_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_role_cache_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_role_cache_state{}} |
    {reply, Reply :: term(), NewState :: #mod_role_cache_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_role_cache_state{}} |
    {noreply, NewState :: #mod_role_cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_role_cache_state{}} |
    {stop, Reason :: term(), NewState :: #mod_role_cache_state{}}).
handle_call(_Request, _From, State = #mod_role_cache_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_role_cache_state{}) ->
    {noreply, NewState :: #mod_role_cache_state{}} |
    {noreply, NewState :: #mod_role_cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_cache_state{}}).
handle_cast(_Request, State = #mod_role_cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_role_cache_state{}) ->
    {noreply, NewState :: #mod_role_cache_state{}} |
    {noreply, NewState :: #mod_role_cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_cache_state{}}).
handle_info(save, State = #mod_role_cache_state{}) ->
    erlang:send_after(60 * 1000, self(), save),
    lib_role_cache:save_role_cache(),
    {noreply, State};
handle_info(_Info, State = #mod_role_cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_role_cache_state{}) -> term()).
terminate(_Reason, _State = #mod_role_cache_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_role_cache_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_role_cache_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_role_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
db_init(State, _Args) ->
    lib_role_cache:load_role_cache(),
    {noreply, State}.

get_pid() ->
    erlang:whereis(?MODULE).