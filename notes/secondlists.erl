-module(secondlists).
-export([wtf/0, head/0, tail/0, cons/2,
         list_comp/1, comp_multi/0, comp_two_lists/0, comp_tuples/0]).

%% Lists [Elem1, Elem2...]

% [1, 2] ++ [3]         [1, 2, 3]
% [1, 2, 3] -- [2]      [1, 3]
% [1,2,3] ++ [4, 5] + [5, 6]
% chaining goes from right to left
% [1,2,3] -- [1,2] -- [2].   -> [2, 3]
% [1, 2] -- [2] = [1],  [1, 2 ,3] -- [1] = [2, 3].

wtf() ->
    [97 ,98, 99].   % "abc" strings are list and Erlang will coerce if possible


head() ->
    hd([1, 2 ,3, 4]).     % 1


tail() ->
    tl([1,2 ,3 , 4]).   % 4


cons(Elem, List) ->
    % adds new head to list
    [Elem|List].    % cons(1, [2, 3, 4]) -> [1, 2 ,3 ,4]

% note this works by pattern matching
% [Head, Tail] = [1, 2 ,3, 4], Head = 1, Tail = [2, 3, 4].

% cons (|) can be used recursively [1 | [ 2 | [3 | []]]] = [1, 2,  3].

% N.B. lists properly end with and empty list. hence[2 | [3]]
% [2 | 3] will work for pattern matching but not with standard functions


%% List Comprehensions

list_comp(List) ->
    % return list of squares of even numbers
    % secondlists:list_comp([1, 2 ,3, 4]). -> [4, 16]
    % [action on Var|| Var <- List, Condition].
    [X * X || X <- List ,X rem 2 =:= 0].

comp_multi() ->
    Paints = [{red, 10}, {blue, 20}, {orange, 30}],
    [
     {Colour, Quantity + 10} || {Colour, Quantity} <- Paints,
                                Colour =/= blue, Quantity < 30
    ].          % -> [{red, 20}]

comp_two_lists() ->
    [X + Y || X <- [1, 2], Y <- [3, 4]].    % [4, 5, 5, 6]

comp_tuples() ->
    Paints = [{red, 10}, {blue, 20}, {orange, 30}, {green, 20}],
    % return lists where 20 matches 2nd element
    % using = would throw an exception
    [Colour || {Colour, 20} <- Paints].   % [blue, green]
