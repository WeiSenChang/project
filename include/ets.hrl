%% coding: utf-8
-author("weisenchang").

-ifndef('ets_HRL').
-define('ets_HRL', true).

-define(ETS_OPTS, [named_table]).
-define(KEY_POS, keypos).

%% 公共ets表
-define(ETS_UID, ets_uid).

%% 角色ets表
-define(ETS_ROLE, ets_role).
-define(ETS_ROLE_OTHER, ets_role_other).
-define(ETS_ROLE_SHOW, ets_role_show).
-define(ETS_ROLE_FIGHT, ets_role_fight).

-endif.
