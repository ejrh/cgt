:- module(hackenbush, []).

cgt:move(H, P, M) :-
    H =.. [hackenbush|_],
    hb_move(H, P, M).

test_state(0, hackenbush).
test_state(1, hackenbush(red)).
test_state(2, hackenbush(red, red(blue))).
test_state(3, hackenbush(blue(red), red(blue))).

player_matches(P, P).
player_matches(P, green).

hb_move(H, P, M) :-
    functor(H, R, _),
    player_matches(P, R),
    M = end.

hb_move(H, P, M) :-
    \+is_list(H),
    H =.. [R|List],
    hb_list_move(List, P, SubM),
    M =.. [R|SubM].

hb_list_move([H|T], P, M) :-
    hb_move(H, P, SubM),
    ((SubM = end) -> (M = T) ; M = [SubM|T]).

hb_list_move([H|T], P, M) :-
    hb_list_move(T, P, SubM),
    M = [H|SubM].
