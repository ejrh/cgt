:- module(splitty, []).

cgt:move(N, _, N2) :-
    N =.. [splitty|H],
    splitty_move(H, H2),
    splitty_normalise(H2, H2Normalised),
    N2 =.. [splitty|H2Normalised].

cgt:winner(Game/Player, Winner) :-
    Game = splitty,
    cgt:enemy(Player, Winner).

cgt:game_label(G, Label) :-
    G =.. [splitty|L],
    findall(Str, (member(X, L), atom_string(X, Str)), Strs),
    atom_string(' ', SP),
    string_join(SP, Strs, Label).

splitty_normalise(H, H2) :-
   msort(H, H2). 

test_state(0, splitty).
test_state(1, splitty(1)).
test_state(2, splitty(1,2)).
test_state(3, splitty(4)).
test_state(4, splitty(5)).

splitty_move([1|T], T).
splitty_move([2|T], T).
splitty_move([H|T], [NewH|T]) :-
    member(S, [1,2]),
    NewH is H - S,
    NewH >= 1.
splitty_move([H|T], [NewH1,NewH2|T]) :-
    Max is (H - 1)//2,
    between(1, Max, S),
    NewH1 is S,
    NewH2 is H - S.

splitty_move([H|T], [H|NewT]) :-
    splitty_move(T, NewT).
