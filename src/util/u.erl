%% -*- coding: utf-8 -*-

-module(u).

-include("common.hrl").

-define(BEAM_PATH, "./ebin/").

%% API
-export([u/0]).

u() ->
    {ok, Files} = file:list_dir(?BEAM_PATH),
    Mods = get_change_mod(Files),
    Reloads = u([], Mods),
    io:format("reloads ~w", [Reloads]),
    ok.
u(Reloads, []) ->
    lists:reverse(Reloads);
u(Reloads, [Mod | Tail]) ->
    try
        code:purge(Mod),
        code:load_file(Mod),
        ?INFO("reload ~w", [Mod]),
        u([Mod | Reloads], Tail)
    catch
        _:_ ->
            ?WARING("reload error: ~w", [Mod]),
            u(Reloads, Tail)
    end.

get_change_mod(Files) ->
    get_change_mod(Files, []).
get_change_mod([], Mods) ->
    Mods;
get_change_mod([File|T], Mods) ->
    NewMods =
    case string:tokens(File, ".") of
        [StrMod, "beam"] ->
            Mod = lib_types:to_atom(StrMod),
            Vsn0 = get_file_vsn(StrMod),
            Vsn1 = get_code_vsn(Mod),
            case Vsn0 =/= Vsn1 of
                true -> [Mod|Mods];
                _ -> Mods
            end;
        _ -> Mods
    end,
    get_change_mod(T, NewMods).

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