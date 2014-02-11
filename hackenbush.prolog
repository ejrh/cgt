:- module(hackenbush, []).

cgt:move(H, P, M) :-
    hb_move(H, P, M).

test_state(0, H) :-
    H = hackenbush.

test_state(1, H) :-
    H = hackenbush(red).

test_state(2, H) :-
    H = hackenbush(red, red(blue)).

test_state(3, H) :-
    H = hackenbush(blue(red), red(blue)).

player_matches(P, P).
player_matches(P, green).

hb_move(H, P, M) :-
    functor(H, R, _),
    player_matches(P, R),
    M = end.

hb_move(H, P, M) :-
    \+is_list(H),
    H =.. [R|List],
    hb_move(List, P, SubM),
    M =.. [R|SubM].

hb_move([H|T], P, M) :-
    hb_move(H, P, SubM),
    ((SubM = end) -> (M = T) ; M = [SubM|T]).

hb_move([H|T], P, M) :-
    hb_move(T, P, SubM),
    M = [H|SubM].
