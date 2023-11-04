%% coding: utf-8
-module(lib_common).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    log_tab/1
]).

%% 根据表名返回当前时间的新表名, 最小单位为1小时
log_tab(Tab) ->
    {{Year, Month, Day}, {Hour, _Min, _}} =
        lib_timer:to_local_time(lib_timer:unix_time()),
    TableName =
        lib_types:to_list(Tab) ++ "_" ++
        lib_types:to_list(Year) ++ "_" ++
        lib_types:to_list(Month) ++ "_" ++
        lib_types:to_list(Day) ++ "_" ++
        lib_types:to_list(Hour),
    lib_types:to_atom(TableName).