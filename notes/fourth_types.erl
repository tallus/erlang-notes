-module('fourth_types').
-export([convert_number/1]).
%% Types
% Erlang is a dynamic and strongly typed.

%% Type conversion
% The following functions can be used for type conversion
% atom_to_binary/2, atom_to_list/1,
% binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1,
% bitstring_to_list/1, binary_to_term/1,
% float_to_list/1, fun_to_list/1, integer_to_list/1, integer_to_list/2,
% iolist_to_binary/1, iolist_to_atom/1,
% list_to_atom/1, list_to_binary/1, list_to_bitstring/1, list_to_existing_atom/1,
% list_to_float/1, list_to_integer/2, list_to_pid/1, list_to_tuple/1,
% pid_to_list/1, port_to_list/1, ref_to_list/1, term_to_binary/1, term_to_binary/2,
% tuple_to_list/1.

%% Type guarding

% is_atom/1           is_binary/1
% is_bitstring/1      is_boolean/1        is_builtin/3
% is_float/1          is_function/1       is_function/2
% is_integer/1        is_list/1           is_number/1
% is_pid/1            is_port/1           is_record/2
% is_record/3         is_reference/1      is_tuple/1

% probably not a good idea
convert_number(N) when is_float(N) ->
    float_to_list(N);
convert_number(N) when is_integer(N) ->
    integer_to_list(N);
convert_number(N) ->
    N.
