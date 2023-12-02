%% -*- coding: utf-8 -*-

-module(friend_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([get_pid/0, get_p_name/0]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(friend_server_state, {}).

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
    {ok, #friend_server_state{}}.

handle_call(_Request, _From, State = #friend_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #friend_server_state{}) ->
    %% do db_init
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    {noreply, State};
handle_cast(_Request, State = #friend_server_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #friend_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #friend_server_state{}) ->
    ok.

code_change(_OldVsn, State = #friend_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
