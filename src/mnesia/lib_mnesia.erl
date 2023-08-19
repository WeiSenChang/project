%% coding: utf-8
-module(lib_mnesia).
-author("weisenchang").

-include("common.hrl").
-include("mnesia.hrl").

%% API
-export([
    init_db/0,
    all_keys/1,
    read/3,
    write/3,
    delete/2
]).

init_db() ->
    init_mnesia(),
    mnesia:start(),
    create_tables().

init_mnesia() ->
    init_mnesia(mnesia:system_info(use_dir)).

init_mnesia(false) ->
    mnesia:create_schema([node()]);
init_mnesia(_) ->
    ignore.

create_tables() ->
    TabL = mnesia:system_info(tables),
    SysTabL = lists:subtract(?DB_GLOBAL_TAB_LIST, TabL),
    create_tables(SysTabL, [{disc_copies, [node()]}]),
    RoleTabL = lists:subtract(?DB_ROLE_TAB_LIST, TabL),
    create_tables(RoleTabL, [{disc_only_copies, [node()]}]).

create_tables(TabL, TabAttrs) ->
    Fun = fun(Tab) -> mnesia:create_table(Tab, TabAttrs) end,
    lists:foreach(Fun, TabL),
    mnesia:wait_for_tables(TabL, 1000).

%%%%%%%%%%%%%%%%%%%%%%%
all_keys(Tab) ->
    Fun = fun() -> mnesia:all_keys(Tab) end,
    all_keys(mnesia:transaction(Fun), Tab).

all_keys({atomic, Keys}, _Tab) -> Keys;
all_keys(_, _Tab) -> [].

read(Tab, Key, Def) ->
    Fun = fun() -> mnesia:read(Tab, Key, read) end,
    read(mnesia:transaction(Fun), Def).

read({atomic, [{_, Data}]}, _Def) -> Data;
read(_, Data) -> Data.

write(Tab, Key, Data) ->
    Fun = fun() -> mnesia:write(Tab, {Key, Data}, write) end,
    mnesia:transaction(Fun).

delete(Tab, Key) ->
    Fun = fun() -> mnesia:delete({Tab, Key}) end,
    mnesia:transaction(Fun).