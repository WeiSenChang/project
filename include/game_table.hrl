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

-include("common.hrl").

tables() ->
    [
        "role",
        "role_friend",

        "uid"
    ].

table("role") ->
    #table{name = "role", key = "id", def = "DB_ROLE", type = ?TAB_TYPE_ROLE,
        fields = [
            #field{name = "id", type = ?INT},
            #field{name = "name", type = ?STRING},
            #field{name = "money", type = ?FLOAT},
            #field{name = "account", type = ?STRING},
            #field{name = "item_list", type = ?LIST, sub_type = "key_value"}
        ]
    };
table("role_friend") ->
    #table{name = "role_friend", key = "id", def = "DB_ROLE_FRIEND", type = ?TAB_TYPE_ROLE,
        fields = [
            #field{name = "id", type = ?INT},
            #field{name = "friend_list", type = ?LIST, sub_type = "friend"},
            #field{name = "apply_list", type = ?LIST, sub_type = ?INT},
            #field{name = "black_list", type = ?LIST, sub_type = ?INT}
        ]
    };
table("uid") ->
    #table{name = "uid", key = "key", def = "DB_UID", type = ?TAB_TYPE_SYS,
        fields = [
            #field{name = "key", type = ?STRING},
            #field{name = "id", type = ?INT}
        ]
    };
table(_) ->
    ?UNDEFINED.


fields("key_value") ->
    [
        #field{name = "key", type = ?INT},
        #field{name = "value", type = ?INT}
    ];
fields(_) ->
    [].

-endif.