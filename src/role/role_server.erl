%% -*- coding: utf-8 -*-

-module(role_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([get_pid/1, get_p_name/1]).

%% gen_server callback
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(role_server_state, {role_id = 0}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid(RoleId) ->
    erlang:whereis(get_p_name(RoleId)).

get_p_name(RoleId) ->
    lib_types:to_atom(lib_types:to_list(?MODULE) ++ "_" ++ lib_types:to_list(RoleId)).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(RoleId) ->
    gen_server:start_link({local, get_p_name(RoleId)}, ?MODULE, [RoleId], []).

init([RoleId]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    {ok, #role_server_state{role_id = RoleId}}.

handle_call(_Request, _From, State = #role_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #role_server_state{role_id = RoleId}) ->
    %% do db_init
    db_init(RoleId),
    lib_role_listen:listen_login(RoleId),
    {noreply, State};
handle_cast({change_name, Name}, State = #role_server_state{role_id = RoleId}) ->
    lib_role:change_name(RoleId, Name),
    {noreply, State};
handle_cast(_Request, State = #role_server_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #role_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #role_server_state{role_id = RoleId}) ->
    lib_role_listen:listen_logout(RoleId),
    db_save(RoleId),
    ok.

code_change(_OldVsn, State = #role_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
db_init(RoleId) ->
    db_init(RoleId, db_table:role_tabs()).
db_init(_RoleId, []) ->
    ok;
db_init(RoleId, [Tab | Tail]) ->
    ok = lib_db:load_and_set_cache(Tab, RoleId),
    db_init(RoleId, Tail).

db_save(RoleId) ->
    db_save(RoleId, db_table:role_tabs()).
db_save(_RoleId, []) ->
    ok;
db_save(RoleId, [Tab | Tail]) ->
    ok = lib_db:save(Tab, RoleId),
    lib_ets:delete(Tab, RoleId),
    db_save(RoleId, Tail).