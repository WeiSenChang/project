%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 8月 2023 1:18
%%%-------------------------------------------------------------------
-module(mod_mnesia).
-author("weisenchang").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, db_init/2, get_pid/0, insert/3, async_insert/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(DUMP_LIST, dump_list).

-record(mod_mnesia_state, {}).

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
    {ok, State :: #mod_mnesia_state{}} | {ok, State :: #mod_mnesia_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:send_after(50, self(), dump),
    erlang:send_after(30 * 1000, self(), gc),
    {ok, #mod_mnesia_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mod_mnesia_state{}) ->
    {reply, Reply :: term(), NewState :: #mod_mnesia_state{}} |
    {reply, Reply :: term(), NewState :: #mod_mnesia_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mod_mnesia_state{}} |
    {noreply, NewState :: #mod_mnesia_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mod_mnesia_state{}} |
    {stop, Reason :: term(), NewState :: #mod_mnesia_state{}}).
handle_call(_Request, _From, State = #mod_mnesia_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mod_mnesia_state{}) ->
    {noreply, NewState :: #mod_mnesia_state{}} |
    {noreply, NewState :: #mod_mnesia_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_mnesia_state{}}).
handle_cast(_Request, State = #mod_mnesia_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mod_mnesia_state{}) ->
    {noreply, NewState :: #mod_mnesia_state{}} |
    {noreply, NewState :: #mod_mnesia_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mod_mnesia_state{}}).
handle_info(dump, State = #mod_mnesia_state{}) ->
    erlang:send_after(50, self(), dump),
    dump_data(),
    {noreply, State};
handle_info(gc, State = #mod_mnesia_state{}) ->
    erlang:send_after(30 * 1000, self(), gc),
    erlang:garbage_collect(self()),
    {noreply, State};
handle_info(_Info, State = #mod_mnesia_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mod_mnesia_state{}) -> term()).
terminate(_Reason, _State = #mod_mnesia_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mod_mnesia_state{},
    Extra :: term()) ->
    {ok, NewState :: #mod_mnesia_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mod_mnesia_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
db_init(State, _Args) ->
    {noreply, State}.

get_pid() ->
    whereis(?SERVER).

insert(Tab, Key, Data) ->
    Pid = mod_mnesia:get_pid(),
    Fun = fun mod_mnesia:async_insert/1,
    Args = [{Tab, Key, Data}],
    mod_server:async_apply(Pid, Fun, Args).

async_insert(Dump) ->
    ?DEBUG("~w", [Dump]),
    DumpList = get_dump_list(),
    put_dump_list([Dump|DumpList]).


%%%%%%
get_dump_list() ->
    case erlang:get(?DUMP_LIST) of
        undefined -> [];
        DumpList -> DumpList
    end.

put_dump_list(DumpList) ->
    erlang:put(?DUMP_LIST, DumpList).

dump_data() ->
    DumpList = get_dump_list(),
    Num = max(0, length(DumpList) - 2000),
    {LeftDumpList, DoDumpList} = lists:split(Num, DumpList),
    put_dump_list(LeftDumpList),
    Fun = fun({Tab, Key, Data}) -> lib_mnesia:write(Tab, Key, Data) end,
    lists:foreach(Fun, lists:reverse(DoDumpList)).