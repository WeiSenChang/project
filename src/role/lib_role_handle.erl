%% coding: utf-8
-module(lib_role_handle).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    get_role_handles/0
]).

get_role_handles() ->
    [
        lib_role:role_handle()
    ].