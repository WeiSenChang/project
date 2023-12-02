%% -*- coding: utf-8 -*-

-ifndef('ets_HRL').
-define('ets_HRL', true).

-define(ETS_OPTS, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]).

-define(ETS_GLOBAL_CACHE, ets_global_cache).

-endif.