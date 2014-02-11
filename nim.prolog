:- module(nim, []).
:- multifile(move/3).

cgt:move(N, P, N2) :-
    N =.. [nim|H],
    nim_move(H, H2),
    N2 =.. [nim|H2].

test_state(0, nim).

test_state(1, nim(1)).

test_state(2, nim(1,2)).

nim_move([H|T], T).

nim_move([H|T], [NewH|T]) :-
    Max is H - 1,
    between(1, Max, NewH).

nim_move([H|T], [H|NewT]) :-
    nim_move(T, NewT).
