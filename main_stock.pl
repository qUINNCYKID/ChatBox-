% main_stock.pl
% Fully working REPL + memory support

:- module(main_stock, [start/0]).

:- use_module(input_processing, [detect_intent/3]).
:- use_module(memory_manager, [process_input/2, recall/3]).
:- use_module(response_generator, [generate_response/5, help_text/1]).

:- dynamic turn_count/1.
turn_count(0).

% --------------------------------------
% ENTRY POINT
% --------------------------------------
start :-
    format("Hi! I'm a modular Stock Market chatbot. Type 'quit' to exit.~n"),
    format("Type 'help' to see what I cover.~n~n"),
    loop.

% --------------------------------------
% MAIN LOOP
% --------------------------------------
loop :-
    write('You: '), flush_output,
    read_line_to_string(user_input, Line),

    ( Line = end_of_file -> Text = "quit" ; Text = Line ),

    % --------------------------------------
    % FIRST: MEMORY MANAGER (name, location)
    % --------------------------------------
    ( memory_manager:process_input(Text, MemReplies),
      MemReplies \= [] ->
          forall(member(R, MemReplies), format("Chatbot: ~w~n", [R])),
          ( Text = "quit" -> true ; loop )
    ; true ),

    % --------------------------------------
    % SPECIAL: RECALL QUERIES
    % --------------------------------------
    string_lower(Text, Lower),

    ( (sub_string(Lower, _, _, _, "who am i") ;
       sub_string(Lower, _, _, _, "what is my name")) ->
        ( recall("name", "user", Name) ->
              format("Chatbot: You told me your name is ~w.~n", [Name])
        ;     format("Chatbot: I don't know your name yet. Tell me by saying 'My name is ...'~n")
        ),
        loop
    ; true ),

    ( (sub_string(Lower, _, _, _, "where do i live") ;
       sub_string(Lower, _, _, _, "where am i from")) ->
        ( recall("location", "home", Loc) ->
              format("Chatbot: You told me you live in ~w.~n", [Loc])
        ;     format("Chatbot: I don't know where you live yet. Tell me by saying 'I live in ...'~n")
        ),
        loop
    ; true ),

    % --------------------------------------
    % NORMAL INTENT HANDLING
    % --------------------------------------
    detect_intent(Text, Intent, Matches),

    ( retract(turn_count(N)) -> N1 is N + 1 ; N1 = 1 ),
    asserta(turn_count(N1)),

    State = state{turn_count:N1},
    generate_response(Intent, Text, Matches, State, Reply),

    format("Chatbot: ~w~n~n", [Reply]),

    ( Intent = quit -> true ; loop ).
