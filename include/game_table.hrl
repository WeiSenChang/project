%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2023 6:26
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('game_table_HRL').
-define('game_table_HRL', true).

-include("db.hrl").

%% db tables
tables() ->
    [
        "db_role",
        "db_role_friend",
        "db_role_cache",

        "db_uid"
    ].


%% db table
table("db_role") ->
    #table{name = "db_role", key = "role_id", def = "DB_ROLE", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT},
        #field{name = "exp", type = ?INT},
        #field{name = "is_online", type = ?INT},
        #field{name = "offline_tick", type = ?INT}
    ]};
table("db_role_friend") ->
    #table{name = "db_role_friend", key = "role_id", def = "DB_ROLE_FRIEND", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "friend_list", type = ?LIST, sub_type = "r_friend"},
        #field{name = "apply_list", type = ?LIST, sub_type = ?INT},
        #field{name = "black_list", type = ?LIST, sub_type = ?INT}
    ]};
table("db_role_cache") ->
    #table{name = "db_role_cache", key = "role_id", def = "DB_ROLE_CACHE", type = ?TAB_TYPE_SYS, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT},
        #field{name = "is_online", type = ?INT},
        #field{name = "offline_tick", type = ?INT}
    ]};
table("db_uid") ->
    #table{name = "db_uid", key = "key", def = "DB_UID", type = ?TAB_TYPE_SYS, fields = [
        #field{name = "key", type = ?STRING},
        #field{name = "id", type = ?INT}
    ]};




%% db record
table("r_kv") ->
    #table{fields = [
        #field{name = "key", type = ?INT},
        #field{name = "value", type = ?INT}
    ]};
table("r_kv_float") ->
    #table{fields = [
        #field{name = "key", type = ?INT},
        #field{name = "float", type = ?FLOAT}
    ]};
table("r_friend") ->
    #table{fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "role_name", type = ?STRING},
        #field{name = "other", type = ?LIST, sub_type = ?STRING}
    ]};
table(_) ->
    #table{}.




-endif.