%% coding: utf-8
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 2:57
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('common_HRL').
-define('common_HRL', true).

-define(INT, int).
-define(FLOAT, float).
-define(STRING, string).
-define(LIST, list).

-define(TRUE, true).
-define(FALSE, false).
-define(UNDEFINED, undefined).

-define(TAB_TYPE_ROLE, "role").
-define(TAB_TYPE_SYS, "sys").

-record(table, {name, key, type, def, load_num, fields}).
-record(field, {name, type, sub_type, value}).

-define(DEBUG(Format), mod_log:msg(?MODULE, ?LINE, Format)).
-define(DEBUG(Format, Args), mod_log:msg(?MODULE, ?LINE, Format, Args)).

-endif.
