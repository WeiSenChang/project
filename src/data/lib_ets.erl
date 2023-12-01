-module(lib_ets).
-author("weisenchang").
-include("common.hrl").
-include("ets.hrl").

%% API
-export([
    ets_init/0,
    set/3,
    get/2,
    get/3
]).

ets_init() ->
    Tabs = db_table:role_tables() ++ db_table:sys_tables(),
    lists:foreach(
        fun(Tab) ->
            #table{ets = Ets, ets_state = EtsState} = db_table:get_table(Tab),
            ets:new(Ets, ?ETS_OPTS),
            ets:new(EtsState, ?ETS_OPTS)
        end, Tabs),
    ok.

get(Ets, Key) ->
    get(Ets, Key, undefined).

get(Ets, Key, Def) ->
    case ets:lookup(Ets, Key) of
        [{Key, Value}] -> Value;
        _ -> Def
    end.

set(Ets, Key, Value) ->
    ets:insert(Ets, {Key, Value}).