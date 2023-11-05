%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2023 10:01
%%%-------------------------------------------------------------------
-module(lib_types).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    to_integer/1,
    to_float/1,
    to_list/1,
    to_atom/1,
    to_binary/1
]).

to_integer(Value) when is_integer(Value) ->
    Value;
to_integer(Value) when is_float(Value) ->
    round(Value);
to_integer(Value) when is_binary(Value) ->
    ?TRY_CATCH(binary_to_integer(Value), ?TRY_CATCH(round(binary_to_float(Value)), 0));
to_integer(Value) when is_list(Value) ->
    ?TRY_CATCH(list_to_integer(Value), ?TRY_CATCH(round(list_to_float(Value)), 0));
to_integer(Value) when is_atom(Value) ->
    BinValue = atom_to_binary(Value),
    to_integer(BinValue);
to_integer(Value) ->
    BinValue = term_to_binary(Value),
    to_integer(BinValue).

to_float(Value) when is_float(Value) ->
    Value;
to_float(Value) when is_integer(Value) ->
    Value + 0.0;
to_float(Value) when is_binary(Value) ->
    ?TRY_CATCH(binary_to_float(Value), ?TRY_CATCH(binary_to_integer(Value) + 0.0, 0));
to_float(Value) when is_list(Value) ->
    ?TRY_CATCH(list_to_float(Value), ?TRY_CATCH(list_to_integer(Value) + 0.0, 0));
to_float(Value) when is_atom(Value) ->
    BinValue = atom_to_binary(Value),
    to_float(BinValue);
to_float(Value) ->
    BinValue = term_to_binary(Value),
    to_float(BinValue).

to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) when is_float(Value) ->
    float_to_list(Value);
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) ->
    binary_to_list(term_to_binary(Value)).

to_atom(Value) when is_atom(Value) ->
    Value;
to_atom(Value) when is_binary(Value) ->
    binary_to_atom(Value);
to_atom(Value) when is_list(Value) ->
    list_to_atom(Value);
to_atom(Value) when is_float(Value) ->
    binary_to_atom(float_to_binary(Value));
to_atom(Value) when is_integer(Value) ->
    binary_to_atom(integer_to_binary(Value));
to_atom(Value) ->
    binary_to_atom(term_to_binary(Value)).

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_float(Value) ->
    float_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value);
to_binary(Value) ->
    term_to_binary(Value).