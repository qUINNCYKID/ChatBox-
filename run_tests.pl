% run_tests.pl -- simple smoke tests for the chatbot modules
% Load modules without importing their exported predicates into the `user` module
% to avoid import name collisions (both modules export `get_scope_demo/1`).
:- use_module('/home/quinb/classspi/prolog/input_processing.pl', [detect_intent/3]).
:- use_module('/home/quinb/classspi/prolog/response_generator.pl', [generate_response/5, help_text/1]).
:- initialization(run_tests).

run_tests :-
    format("Running smoke tests...~n~n"),
    test_detect_intent("Hello"),
    test_detect_intent("what is a stock"),
    test_detect_intent("quit"),
    test_generate(help),
    test_generate(stock),
    format("All tests finished.~n"),
    halt.

test_detect_intent(Text) :-
    format("== detect_intent(\"~w\")~n", [Text]),
    ( input_processing:detect_intent(Text, Intent, Matches) ->
        format("Intent: ~w~nMatches: ~w~n~n", [Intent, Matches])
    ;   format("detect_intent/3 failed for ~w~n~n", [Text])
    ).

test_generate(Intent) :-
    format("== generate_response(~w)~n", [Intent]),
    ( response_generator:generate_response(Intent, "", [], state{turn_count:1}, Reply) ->
        format("Reply: ~w~n~n", [Reply])
    ;   format("generate_response/5 failed for ~w~n~n", [Intent])
    ).
