%% coding: utf-8
-module(lib_test_cmd).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    role_gm/5
]).

role_gm(create, Account, Roles, _Par3, _Par4) ->
    [lib_role_login:create(Account) || _ <- lists:seq(1, Roles)];

role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?WARNING("no role gm: ~w", [Gm]).