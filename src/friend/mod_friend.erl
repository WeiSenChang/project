%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 8月 2023 6:38
%%%-------------------------------------------------------------------
-module(mod_friend).
-author("weisenchang").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, get_pid/0, stop/0, db_init/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mod_friend_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mod_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_friend_state{}} | {ok, State :: #mod_friend_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTING),
    {ok, #mod_friend_state{}}.

db_init(State = #mod_friend_state{}) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTED),
    {noreply, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_friend_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_friend_state{}} |
    {reply, Reply :: term(), NewState :: #mod_friend_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_friend_state{}} |
    {noreply, NewState :: #mod_friend_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_friend_state{}} |
    {stop, Reason :: term(), NewState :: #mod_friend_state{}}).
handle_call(_Request, _From, State = #mod_friend_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_friend_state{}) ->
    {noreply, NewState :: #mod_friend_state{}} |
    {noreply, NewState :: #mod_friend_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_friend_state{}}).
handle_cast(_Request, State = #mod_friend_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_friend_state{}) ->
    {noreply, NewState :: #mod_friend_state{}} |
    {noreply, NewState :: #mod_friend_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_friend_state{}}).
handle_info(_Info, State = #mod_friend_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_friend_state{}) -> term()).
terminate(_Reason, _State = #mod_friend_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_friend_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_friend_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_friend_state{}, _Extra) ->
    mod_server:put_callback_mod(?MODULE),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_pid() ->
    erlang:whereis(?SERVER).

stop() ->
    mod_server:sync_stop(get_pid()).