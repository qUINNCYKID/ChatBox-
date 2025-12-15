% memory_manager.pl
:- module(memory_manager, [
    process_input/2,
    recall/3
]).

:- dynamic fact/3.

% Handle inputs like:
% "My name is John"
% "I live in Chicago"
process_input(Input, Replies) :-
    string_lower(Input, Lower),

    (   sub_string(Lower, _, _, _, "my name is") ->
            extract_value(Input, "my name is", Name),
            retractall(fact("name", "user", _)),
            assertz(fact("name", "user", Name)),
            Replies = ["Okay, I’ll remember your name."]

    ;   sub_string(Lower, _, _, _, "i live in") ->
            extract_value(Input, "i live in", Loc),
            retractall(fact("location", "home", _)),
            assertz(fact("location", "home", Loc)),
            Replies = ["Got it — I’ll remember where you live."]

    ;   Replies = []
    ).

% Extract the value after a keyword
extract_value(Input, Keyword, Value) :-
    sub_string(Input, Start, _, _, Keyword),
    string_length(Keyword, Len),
    Pos is Start + Len,
    sub_string(Input, Pos, _, 0, RawValue),
    normalize_space(string(Value), RawValue).

recall(Type, Key, Value) :-
    fact(Type, Key, Value).
