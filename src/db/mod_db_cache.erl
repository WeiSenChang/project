%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(mod_db_cache).

-behaviour(gen_server).

-include("db.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mod_db_cache_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Tabs = db_table:role_tables() ++ db_table:sys_tables(),
    lists:foreach(fun(Tab) -> ets:new(?ETS(Tab), [named_table, set, public]) end, Tabs),
    {ok, #mod_db_cache_state{}}.

handle_call(_Request, _From, State = #mod_db_cache_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #mod_db_cache_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #mod_db_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #mod_db_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #mod_db_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================