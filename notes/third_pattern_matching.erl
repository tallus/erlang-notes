-module(third_pattern_matching).
-export([paint/2, valid_date/1, age_guard/1, age_if/1, better_if/1, insert/2,
         eat/1]).

%% Basic Pattern Matching.
% Rather than use if/else or case statements pattern matching can be use in erlang

paint(red, Thing) ->
    io:format("The ~s is painted red~n", [Thing]);
paint(blue, Thing) ->
    io:format("The ~s is painted a lovely shade of blue~n", [Thing]);
paint(green, Thing) ->
    io:format("The ~s is painted a disturbing shade of green~n", [Thing]);
% default
paint(_, Thing) ->
    io:format("This is not a suitable colour for a ~s~n", [Thing]).

%% Checking arguments with =
% You can perform simple checking to ensure arguments are the right shape with =

valid_date(Date = {Y, M, D}) ->
    io:format("~p: It is day ~p in the month ~p of the year ~p~n", [Date, D, M, Y]);
valid_date(_) ->
    io:format("Does not compute~n").

%% Guards
% Guards allow for more sophisticated argument checking by including
% a statement that evaluates to true
%
age_guard(Num) when Num >= 13, Num =<18 ->
    io:format('Teenager~n');
age_guard(Num) when Num < 13 ->
    io:format('Child~n');
age_guard(_) ->
    io:format('Adult~n').

%% If statements
% If statements act as guards inside functions

age_if(Num) ->
    Age = if Num >= 13,  Num =<18 -> 'Teenager';
        Num < 13 -> 'Child';
        true  -> 'Adult'
    end,
    io:format('~s~n', [Age]).

% note true is the equivalent of else, but is is considered better form to avoid it
% by ensuring all cases are covered

better_if(Num) ->
    Age = if Num >= 13,  Num =<18 -> 'Teenager';
        Num < 13 -> 'Child';
        Num > 18  -> 'Adult'
    end,
    io:format('~s~n', [Age]).

%% case of
% another form of pattern matching

% inserts X into list if the list is empty or it does not contain X
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.

eat(Food) ->
    case Food of
        {vegetarian, Heat} when Heat > 5, Heat < 10 ->
            'cautiously';
        {vegetarian, Heat} when Heat < 5 ->
            'definately';
        _  ->
            'never'
    end.
