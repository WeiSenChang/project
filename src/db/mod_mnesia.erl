%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mnesia).

-behaviour(gen_server).

-include("common.hrl").
-include("db.hrl").

-export([start_link/0, get_pid/0, db_init/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(LAST_SAVE_TICK, last_save_tick).

-record(mod_mnesia_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    mod_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_pid() ->
    erlang:whereis(?SERVER).

init([]) ->
    erlang:process_flag(trap_exit, true),
    Tabs = db_table:role_tables() ++ db_table:sys_tables(),
    lists:foreach(
        fun(Tab) ->
            ets:new(?CACHE(Tab), [named_table, public]),
            ets:new(?CACHE_STATE(Tab), [named_table, public])
        end, Tabs),
    erlang:send_after(30 * 1000, self(), save_cache),
    lib_server:set_server_state(?SERVER, ?SERVER_STARTING),
    {ok, #mod_mnesia_state{}}.

db_init(State = #mod_mnesia_state{}) ->
    lib_server:set_server_state(?SERVER, ?SERVER_STARTED),
    {noreply, State}.

handle_call(_Request, _From, State = #mod_mnesia_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #mod_mnesia_state{}) ->
    {noreply, State}.

handle_info(save_cache, State = #mod_mnesia_state{}) ->
    erlang:send_after(30 * 1000, self(), save_cache),
    save_cache(),
    {noreply, State};
handle_info(_Info, State = #mod_mnesia_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #mod_mnesia_state{}) ->
    save_all_cache(),
    ok.

code_change(_OldVsn, State = #mod_mnesia_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_cache() ->
    Tabs = db_table:role_tables() ++ db_table:sys_tables(),
    NowTick = lib_timer:unix_time(),
    [begin
         #table{save_secs = Secs} = db_table:get_table(Tab),
         LastSaveTick = get_last_save_tick(Tab),
         case NowTick >= LastSaveTick + Secs andalso Secs > 0 of
             true ->
                 set_last_save_tick(Tab, NowTick),
                 db_mnesia:save_cache(Tab);
             _ ->
                 ignore
         end
     end || Tab <- Tabs].

save_all_cache() ->
    Tabs = db_table:role_tables() ++ db_table:sys_tables(),
    lists:foreach(fun(Tab) -> db_mnesia:save_cache(Tab) end, Tabs).

get_last_save_tick(Tab) ->
    case erlang:get({?LAST_SAVE_TICK, Tab}) of
        undefined -> 0;
        LastSaveTick -> LastSaveTick
    end.

set_last_save_tick(Tab, Tick) ->
    erlang:put({?LAST_SAVE_TICK, Tab}, Tick).