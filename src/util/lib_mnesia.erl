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
    write/2,
    delete/2
]).

init_db() ->
    case mnesia:system_info(use_dir) of
        false -> mnesia:create_schema([node()]);
        _ -> ignore
    end,
    mnesia:start(),
    TabL = mnesia:system_info(tables),
    create_global_tables(TabL),
    create_role_tables(TabL),
    ok.

create_global_tables(TabL) ->
    create_tables(?DB_GLOBAL_TAB_LIST, TabL, {disc_copies, [node()]}).

create_role_tables(TabL) ->
    create_tables(?DB_ROLE_TAB_LIST, TabL, {disc_only_copies, [node()]}).

create_tables([], _TabL, _TabAttr) ->
    ok;
create_tables([Tab|T], TabL, TabAttr) ->
    case lists:member(Tab, TabL) of
        true -> ignore;
        _ ->
            mnesia:create_table(Tab, [TabAttr|?DB_OPTS]),
            mnesia:wait_for_tables([Tab], 1000)
    end,
    create_tables(T, TabL, TabAttr).

%%%%%%%%%%%%%%%%%%%%%%%
all_keys(Tab) ->
    F = fun() -> mnesia:all_keys(Tab) end,
    case mnesia:transaction(F) of
        {atomic, Keys} -> Keys;
        _ -> []
    end.

read(Tab, Key, Def) ->
    F = fun() -> mnesia:read(Tab, Key, read) end,
    case mnesia:transaction(F) of
        {atomic, [#key_value{key = Key, value = Data}]} -> Data;
        _ -> Def
    end.

write(Tab, Data) ->
    F = fun() -> mnesia:write(Tab, Data, write) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, {no_exists, Tab}} ->
            Attr = case lists:member(Tab, ?DB_GLOBAL_TAB_LIST) of
                       true -> disc_copies;
                       _ -> disc_only_copies
                   end,
            mnesia:create_table(Tab, [{Attr, [node()]}|?DB_OPTS]),
            mnesia:wait_for_tables([Tab], 1000),
            mnesia:transaction(F);
        _Error -> ?WARNING("write error, tab: ~w, data: ~w, error: ~w", [Tab, Data, _Error])
    end.

delete(Tab, Key) ->
    F = fun() -> mnesia:delete({Tab, Key}) end,
    mnesia:transaction(F).