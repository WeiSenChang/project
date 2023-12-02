%% -*- coding: utf-8 -*-

-module(lib_rand).

-include("common.hrl").

%% API
-export([
    rand_from_list/2,
    rand_from_list/3
]).

rand_from_list(List, Num) ->
    rand_from_list(List, Num, true).
rand_from_list(List, Num, true) ->
    Len = length(List),
    lists:foldl(
        fun(_, Acc) ->
            N = rand:uniform(Len),
            [lists:nth(N, List)|Acc]
        end, [], lists:seq(1, Num));
rand_from_list(List, Num, false) ->
    Len = length(List),
    {RandList, _NewList} = lists:foldl(
        fun(_, {AccList0, AccList1}) ->
            N = rand:uniform(Len),
            Term = lists:nth(N, List),
            {[Term|AccList0], lists:delete(Term, AccList1)}
        end, {[], List}, lists:seq(1, Num)),
    RandList;
rand_from_list(List, Num, _IsDup) ->
    rand_from_list(List, Num, false).