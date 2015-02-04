:- module(gsum, []).

cgt:move(S, P, S2) :-
    S =.. [gsum|H],
    gsum_move(H, P, H2),
    S2 =.. [gsum|H2].

cgt:winner(Game/Player, Winner) :-
    Game =.. [gsum|_],
    \+cgt:move(Game, Player, _),
    cgt:enemy(Player, Winner).

gsum_move([H|T], P, [H2|T]) :-
    cgt:move(H, P, H2).

gsum_move([H|T], P, [H|T2]) :-
    gsum_move(T, P, T2).
