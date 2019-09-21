% comments start with a 

% this makes a file an importable module.
% module names are lowercase i.e. an atom
% they can be compiled in the erlang shell with c(module) after cd('this/dir').
-module(first).

% this exports functions for use
% -export([function/arity (number of args), myfunc/1]).
-export([example_func/2, assignment/0, numbers/0, comparisons/0, tuple_type/0]).

%% Syntax
% Statements terminate with a .
% syntax roughly follows conventional punctuation in its use of .,;

%% Functions

example_func(X, Y) ->
    X + Y.      % returns is explicit.
                % note statements in functions are typically separated with
                % commas and the function ends with a full stop

%% Variables
%  Variables start with a capital letter (typically immutable)
% Assignment is done with =, note this also compares
assignment() ->
    % One.    % unbound
    One = 1,
    Two = 2,
    % Three = 3,
    Two = One + One.    % true so returns 2.
    % Three = One + One. not true, so an error would be raised (and warned of)
    % this effectively prevents reassignment

%% Atoms
%  atoms are lowercase, typically, and/or quoted if they do not start lowercase
%  or contain anything other than alphanumeric, underscore or @
%  atoms are effectivly constants (hence use for func names etc)
%  there is a hard limit. Do not generate dynamically.

%% Types - numbers
%  int and floats can be intermingled - Erlang is dynamically typed
% normal precendence rules apply
numbers() ->
    _ = 2 + 5,      % the _ is the unbound variable, (simlar to python)
    _ = 50 * 10,    % here it's used to ignore these statements/supress warning
    _ = 5 / 2,      % 2.5
    _ = 5 div 2,    % 2 - integer division
    _ = 5 rem 2,    % 1 - modulo/remainder
    2#101.      % 5 in base 2

%% Comparison

comparisons() ->
    true and false,         % note these are just atoms not booleans
    true or false,          % short-circuit: only right hand side evaluated
    true xor false,         % true andalso false
    not false,              % true orelse false
    not(true and true),
    % equality/inequality
    5 =:= 5,                % true
    1 =:= 0,                % false
    1 =/= 0,                % true
    5 =:= 5.0,              % false
    5 == 5.0,               % true
    5 /= 5.0,               % false
    % other comparisons
    1 < 2,
    2 > 1,
    1 >= 1,
    1 =< 1.

    % 5 + llama, 5 == llama. exception!
    % 5 =:= true.   false! types don't match so false.
    % 0 == false.   false!
    % 1 < false.    true!
    % number < atom < reference < fun < port < pid < tuple < list < bit string

%% Tuples
% tuples! designated with {}, pythonish. Members can be any type, has any length

tuple_type() ->
    A = 10, B = 20,
    Point = {A, B}.
    X, _ = Point,
    X.              % 10
    % tagged tuples
    % Paint = {red, 10},
    % {blue, 10} = Paint. Exception error: no match on right hand side
