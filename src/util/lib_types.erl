%% -*- coding: utf-8 -*-

-module(lib_types).

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
    erlang:round(Value);
to_integer(Value) when is_binary(Value) ->
    case catch erlang:binary_to_integer(Value) of
        Int when is_integer(Int) -> Int;
        _Err1 ->
            case catch erlang:binary_to_float(Value) of
                Float when is_float(Float) -> erlang:round(Float);
                _Err2 -> throw({error, bad_type})
            end
    end;
to_integer(Value) when is_list(Value) ->
    to_integer(erlang:list_to_binary(Value));
to_integer(Value) when is_atom(Value) ->
    to_integer(m_atom_to_binary(Value));
to_integer(Value) ->
    to_integer(erlang:term_to_binary(Value)).

to_float(Value) when is_float(Value) ->
    Value;
to_float(Value) when is_integer(Value) ->
    Value + 0.0;
to_float(Value) when is_binary(Value) ->
    case catch erlang:binary_to_float(Value) of
        Float when is_float(Float) -> Float;
        _Err1 ->
            case catch erlang:binary_to_integer(Value) of
                Int when is_integer(Int) -> Int + 0.0;
                _Err2 -> throw({error, bad_type})
            end
    end;
to_float(Value) when is_list(Value) ->
    to_float(erlang:list_to_binary(Value));
to_float(Value) when is_atom(Value) ->
    to_float(m_atom_to_binary(Value));
to_float(Value) ->
    to_float(erlang:term_to_binary(Value)).

to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_binary(Value) ->
    erlang:binary_to_list(Value);
to_list(Value) when is_integer(Value) ->
    erlang:integer_to_list(Value);
to_list(Value) when is_float(Value) ->
    erlang:float_to_list(Value);
to_list(Value) when is_atom(Value) ->
    erlang:atom_to_list(Value);
to_list(Value) ->
    erlang:binary_to_list(erlang:term_to_binary(Value)).

to_atom(Value) when is_atom(Value) ->
    Value;
to_atom(Value) when is_binary(Value) ->
    m_binary_to_atom(Value);
to_atom(Value) when is_list(Value) ->
    erlang:list_to_atom(Value);
to_atom(Value) when is_float(Value) ->
    m_binary_to_atom(erlang:float_to_binary(Value));
to_atom(Value) when is_integer(Value) ->
    m_binary_to_atom(erlang:integer_to_binary(Value));
to_atom(Value) ->
    m_binary_to_atom(erlang:term_to_binary(Value)).

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    erlang:list_to_binary(Value);
to_binary(Value) when is_float(Value) ->
    erlang:float_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    m_atom_to_binary(Value);
to_binary(Value) ->
    erlang:term_to_binary(Value).

m_atom_to_binary(Value) ->
    erlang:atom_to_binary(Value, utf8).

m_binary_to_atom(Value) ->
    erlang:binary_to_atom(Value, utf8).