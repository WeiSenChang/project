%% coding: utf-8
-module(u).
-author("weisenchang").

-include("common.hrl").

-define(BEAM_PATH, "./ebin/").

%% API
-export([
    u/0
]).

u() ->
    ?DEBUG("=== code reload begin ==="),
    {ok, FileList} = file:list_dir(?BEAM_PATH),
    Mods = get_change_mod(FileList),
    u(Mods),
    ?DEBUG("=== code reload end   ==="),
    ok.

u([]) -> ok;
u([Mod|T]) ->
    code:purge(Mod),
    code:load_file(Mod),
    ?DEBUG("reload ~p", [Mod]),
    u(T).

get_change_mod(FileList) ->
    get_change_mod(FileList, []).
get_change_mod([], Mods) ->
    Mods;
get_change_mod([File|T], Mods) ->
    Mods1 =
    case string:tokens(File, ".") of
        [StrMod, "beam"] ->
            Mod = erlang:list_to_atom(StrMod),
            Vsn0 = get_file_vsn(StrMod),
            Vsn1 = get_code_vsn(Mod),
            case Vsn0 =/= Vsn1 of
                true -> [Mod|Mods];
                _ -> Mods
            end;
        _ -> Mods
    end,
    get_change_mod(T, Mods1).

get_file_vsn(File) ->
    case beam_lib:version(?BEAM_PATH++File) of
        {ok, {_Mod, [Vsn]}} -> Vsn;
        _ -> undefined
    end.

get_code_vsn(Mod) ->
    Attr = Mod:module_info(attributes),
    case lists:keyfind(vsn, 1, Attr) of
        {vsn, [Vsn]} -> Vsn;
        _ -> undefined
    end.