%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 15:40
%%%-------------------------------------------------------------------
-module(mod_role_manage).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([start_link/0, get_pid/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mod_role_manage_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mod_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_pid() ->
    erlang:whereis(?MODULE).

stop() ->
    mod_server:sync_stop(get_pid()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_role_manage_state{}} | {ok, State :: #mod_role_manage_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:send_after(60 * 1000, self(), save_data),
    [db_mnesia:set_data(RoleCache) || RoleCache <- lib_role_manage:load_all_role_cache()],
    {ok, #mod_role_manage_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_role_manage_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_role_manage_state{}} |
    {reply, Reply :: term(), NewState :: #mod_role_manage_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_role_manage_state{}} |
    {noreply, NewState :: #mod_role_manage_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_role_manage_state{}} |
    {stop, Reason :: term(), NewState :: #mod_role_manage_state{}}).
handle_call(_Request, _From, State = #mod_role_manage_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_role_manage_state{}) ->
    {noreply, NewState :: #mod_role_manage_state{}} |
    {noreply, NewState :: #mod_role_manage_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_manage_state{}}).
handle_cast(_Request, State = #mod_role_manage_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_role_manage_state{}) ->
    {noreply, NewState :: #mod_role_manage_state{}} |
    {noreply, NewState :: #mod_role_manage_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_manage_state{}}).
handle_info(save_data, State = #mod_role_manage_state{}) ->
    erlang:send_after(60 * 1000, self(), save_data),
    db_mnesia:save_data(),
    {noreply, State};
handle_info(_Info, State = #mod_role_manage_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_role_manage_state{}) -> term()).
terminate(_Reason, _State = #mod_role_manage_state{}) ->
    db_mnesia:save_data(),
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_role_manage_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_role_manage_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_role_manage_state{}, _Extra) ->
    mod_server:put_callback_mod(?MODULE),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
