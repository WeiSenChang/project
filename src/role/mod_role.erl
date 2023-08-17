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

-include("role.hrl").
-include("common.hrl").

%% API
-export([get_process_name/1, start_link/1, get_pid/1, db_init/2, stop/1]).

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
    ProcessName = erlang:atom_to_list(?MODULE) ++ "_" ++ erlang:integer_to_list(Id),
    erlang:list_to_atom(ProcessName).

stop(Id) ->
    save_role_data(),
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
    erlang:send_after(60 * 1000, self(), save),
    {ok, #mod_role_state{}}.

db_init(State, [Id]) ->
    load_role_data(Id),
    lib_role_listen:listen_role_login(),
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
handle_info(save, State = #mod_role_state{}) ->
    erlang:send_after(60 * 1000, self(), save),
    save_role_data(),
    {noreply, State};
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
save_role_data() ->
    RoleHandles = lib_role_handle:get_role_handles(),
    save_role_data(RoleHandles).
save_role_data([]) ->
    ok;
save_role_data([RoleHandle|T]) ->
    #role_handle{ets = Ets, get_func = GetFunc, save_func = SaveFunc} = RoleHandle,
    Flag = lib_role_flag:get_ets_cache_flag(Ets),
    case Flag of
        1 ->
            lib_role_flag:put_ets_cache_flag(Ets, 0),
            Data = GetFunc(),
            SaveFunc(Data);
        _ -> ignore
    end,
    save_role_data(T).

load_role_data(RoleId) ->
    RoleHandles = lib_role_handle:get_role_handles(),
    lists:foreach(
        fun(#role_handle{load_func = LoadFunc, put_func = PutFunc}) ->
            Data = LoadFunc(RoleId),
            PutFunc(Data, false)
        end, RoleHandles).
