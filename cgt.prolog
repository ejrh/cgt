:- module(cgt, [move/3]).
:- multifile move/3.

moves(Game, Player, Moves) :-
    findall(M, move(Game, Player, M), Moves).

value(Game, 0) :-
    moves(Game, blue, []),
    moves(Game, red, []).

value(Game, N) :-
    moves(Game, red, []),
    has_move_with_value(Game, blue, SubN),
    N is SubN + 1.

value(Game, N) :-
    moves(Game, blue, []),
    has_move_with_value(Game, red, SubN),
    sub1(SubN, N).

value(Game, N) :-
    has_move_with_value(Game, red, 0),
    has_move_with_value(Game, blue, SubN),
    halve(SubN, N).

value(Game, N) :-
    has_move_with_value(Game, blue, 0),
    has_move_with_value(Game, red, SubN),
    halve(SubN, N).

value(Game, red, N) :-
    value(Game, N).
value(Game, blue, -N) :-
    value(Game, N),
    N \= -_.
value(Game, blue, N) :-
    value(Game, -N).

has_move_with_value(Game, Player, N) :-
    best_move(Game, Player, _, N).

best_move(Game, Player, Move, Value) :-
    findall((NV,M), (move(Game, Player, M), value(M, Player, V), NV is V), MoveValues),
    sort(MoveValues, [(_,Move)|_]),
    value(Move, Value).

halve(1, 1/2).
halve(-(1), -(1/2)).
halve(1/N, 1/N2) :-
    N2 is 2 * N.
halve(-(1/N), -(1/N2)) :-
    N2 is 2 * N.

sub1(0, -(1)).
sub1(-N, -N2) :-
    N2 is N + 1.
