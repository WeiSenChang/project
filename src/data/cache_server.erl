%% -*- coding: utf-8 -*-

-module(cache_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").
-include("db.hrl").

%% API
-export([get_pid/0, get_p_name/0]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOOP_SEC, 10).

-record(cache_server_state, {}).

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
    erlang:send_after(?LOOP_SEC * 1000, self(), loop),
    {ok, #cache_server_state{}}.

handle_call(_Request, _From, State = #cache_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #cache_server_state{}) ->
    %% do db_init
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    {noreply, State};
handle_cast(_Request, State = #cache_server_state{}) ->
    {noreply, State}.

handle_info(loop, State = #cache_server_state{}) ->
    erlang:send_after(?LOOP_SEC * 1000, self(), loop),
    save(db_table:role_tabs()),
    save(db_table:sys_tabs()),
    {noreply, State};
handle_info(_Info, State = #cache_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #cache_server_state{}) ->
    ok.

code_change(_OldVsn, State = #cache_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save([]) ->
    ok;
save([Tab | Tail]) ->
    case db_table:get_table(Tab) of
        #table{secs = Secs} when Secs > 0 ->
            Fun = fun({Key, {S, T, B}}, Acc) -> save(Tab, Key, S, T, B, Secs), Acc end,
            ets:foldl(Fun, ok, Tab);
        _ -> ignore
    end,
    save(Tail).

save(Tab, Key, ?CACHE_SAVE, T, B, S) ->
    case lib_time:unix_time() >= T + S of
        true -> lib_db:save(Tab, Key, {?CACHE_SAVE, T, B});
        false -> ignore
    end;
save(_Tab, _Key, _S, _T, _B, _Secs) ->
    ignore.