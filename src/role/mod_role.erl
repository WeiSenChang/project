%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 18:00
%%%-------------------------------------------------------------------
-module(mod_role).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([get_process_name/1, start_link/1, get_pid/1, db_init/2, stop/1, logout/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(mod_role_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Args :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id) ->
    ProcessName = get_process_name(Id),
    mod_server:start_link({local, ProcessName}, ?MODULE, [Id], []).

get_pid(Id) ->
    ProcessName = get_process_name(Id),
    erlang:whereis(ProcessName).

get_process_name(Id) ->
    ProcessName = lib_types:to_list(?MODULE) ++ "_" ++ lib_types:to_list(Id),
    lib_types:to_atom(ProcessName).

stop(Id) ->
    mod_server:sync_stop(get_pid(Id)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mod_role_state{}} | {ok, State :: #mod_role_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([_RoleId]) ->
    {ok, #mod_role_state{}}.

db_init(State, [Id]) ->
    load_role_data(Id),
    lib_role_listen:listen_role_login(Id),
    {noreply, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_role_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_role_state{}} |
    {reply, Reply :: term(), NewState :: #mod_role_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_role_state{}} |
    {noreply, NewState :: #mod_role_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_role_state{}} |
    {stop, Reason :: term(), NewState :: #mod_role_state{}}).
handle_call(_Request, _From, State = #mod_role_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_role_state{}) ->
    {noreply, NewState :: #mod_role_state{}} |
    {noreply, NewState :: #mod_role_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_state{}}).
handle_cast(_Request, State = #mod_role_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_role_state{}) ->
    {noreply, NewState :: #mod_role_state{}} |
    {noreply, NewState :: #mod_role_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_role_state{}}).
handle_info(_Info, State = #mod_role_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_role_state{}) -> term()).
terminate(_Reason, _State = #mod_role_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_role_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_role_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_role_state{}, _Extra) ->
    mod_server:put_callback_mod(?MODULE),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_role_data(Id) ->
    load_role_data(Id, db_table:role_tables()).
load_role_data(_Id, []) ->
    ok;
load_role_data(Id, [Tab|T]) ->
    Data = db_mnesia:load_data(Tab, Id),
    db_mnesia:set_data(Data),
    load_role_data(Id, T).

logout(Id) ->
    lib_role_listen:listen_role_logout(Id).