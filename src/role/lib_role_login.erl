%% coding: utf-8
-module(lib_role_login).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    create/1,
    login/1,
    logout/1
]).

create(Account) ->
    Id = lib_counter:get_role_id(),
    Role = #role{id = Id, account = Account},
    db_role:save_role(Role),
    #player_account{role_list = RoleList} = PlayerAccount = db_role:load_player_account(Account),
    db_role:save_player_account(PlayerAccount#player_account{role_list = [Role|RoleList]}),
    login(Id).

login(Id) ->
    case server_sup:start_child(mod_role:get_process_name(Id), mod_role, transient, [Id]) of
        {ok, _Pid} -> ok;
        _Reason ->
            ?WARNING("role ~p login error ~p", [Id, _Reason]),
            case logout(Id) of
                ok -> skip;
                _ -> server_sup:delete_child(mod_role:get_process_name(Id))
            end,
            login(Id)
    end.

logout(Id) ->
    case mod_role:get_pid(Id) of
        Pid when is_pid(Pid) ->
            lib_role_listen:listen_role_logout(Id),
            mod_role:stop(Id),
            server_sup:delete_child(mod_role:get_process_name(Id)),
            ok;
        _Error -> ?WARNING("role ~p logout error: ~w", [Id, _Error]), fail
    end.