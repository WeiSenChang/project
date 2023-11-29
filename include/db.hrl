%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2023 20:13
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('db_HRL').
-define('db_HRL', true).

-define(INT, int).
-define(FLOAT, float).
-define(STRING, string).
-define(LIST, list).

-define(TAB_TYPE_ROLE, "role").     %% role process table
-define(TAB_TYPE_SYS, "sys").       %% sys process table

-define(SAVE_MAP, save_map).

-define(ETS(Tab), lib_types:to_atom(lib_types:to_list(Tab) ++ "_ets")).

-record(table, {name, key, type, def, fields = []}).
-record(field, {name, type, sub_type, value}).

-endif.