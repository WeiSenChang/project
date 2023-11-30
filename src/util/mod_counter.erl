%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(mod_counter).

-behaviour(gen_server).

-include("common.hrl").
-include("db_table.hrl").

-export([start_link/0, db_init/1, get_pid/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mod_counter_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    mod_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get_pid() ->
    erlang:whereis(?SERVER).

init([]) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTING),
    {ok, #mod_counter_state{}}.

db_init(State = #mod_counter_state{}) ->
    db:load_cache(?DB_UID),
    lib_server:set_server_state(?SERVER, ?SERVER_STARTED),
    {noreply, State}.

handle_call(_Request, _From, State = #mod_counter_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #mod_counter_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #mod_counter_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #mod_counter_state{}) ->
    ok.

code_change(_OldVsn, State = #mod_counter_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
