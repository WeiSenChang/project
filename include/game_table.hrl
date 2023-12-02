%% -*- coding: utf-8 -*-

-ifndef('game_table_HRL').
-define('game_table_HRL', true).

-include("db.hrl").

%% db tables
tables() ->
    [
        "db_role",
        "db_role_friend",
        "db_role_show",
        "db_count"
    ].


%% table
table("db_role") ->
    #table{key = "role_id", type = ?ROLE_TAB, secs = ?SAVE_SECS, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT},
        #field{name = "exp", type = ?INT},
        #field{name = "login_tick", type = ?INT},
        #field{name = "logout_tick", type = ?INT}
    ]};
table("db_role_friend") ->
    #table{key = "role_id", type = ?ROLE_TAB, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "friend_list", type = ?LIST, sub_type = "r_friend"},
        #field{name = "apply_list", type = ?LIST, sub_type = ?INT},
        #field{name = "black_list", type = ?LIST, sub_type = ?INT}
    ]};
table("db_role_show") ->
    #table{key = "role_id", type = ?SYS_TAB, secs = ?SAVE_SECS, fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT}
    ]};
table("db_count") ->
    #table{key = "key", type = ?SYS_TAB, fields = [
        #field{name = "key", type = ?STRING},
        #field{name = "value", type = ?INT}
    ]};
table(_) ->
    #table{}.




%% record
record("r_kv") ->
    #table{fields = [
        #field{name = "key", type = ?INT},
        #field{name = "value", type = ?INT}
    ]};
record("r_kv_float") ->
    #table{fields = [
        #field{name = "key", type = ?INT},
        #field{name = "float", type = ?FLOAT}
    ]};
record("r_friend") ->
    #table{fields = [
        #field{name = "role_id", type = ?INT},
        #field{name = "role_name", type = ?STRING},
        #field{name = "other", type = ?LIST, sub_type = ?STRING}
    ]};
record(_) ->
    #table{}.




-endif.