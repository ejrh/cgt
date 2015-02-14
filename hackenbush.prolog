:- module(hackenbush, []).

cgt:move(H, P, M) :-
    H =.. [hackenbush|_],
    hb_move(H, P, M, _).

cgt:winner(Game/Player, Winner) :-
    Game =.. [hackenbush|_],
    \+cgt:move(Game, Player, _),
    cgt:enemy(Player, Winner).

test_state(0, hackenbush).
test_state(1, hackenbush(red)).
test_state(2, hackenbush(red, red(blue))).
test_state(3, hackenbush(blue(red), red(blue))).
test_state(4, hackenbush(blue(red(1)), 1)).
random_test_state(H) :-
    tree:random_tree(6, T),
    tree:random_edges(T, [red, blue, green], T2),
    T2 =.. [_|L],
    H =.. [hackenbush|L].

player_matches(P, P).
player_matches(_P, green).

hb_move(H, P, end, Residue) :-
    functor(H, R, _),
    player_matches(P, R),
    get_keys(H, Keys),
    sort(Keys, UniqueKeys),
    Residue = residue(H, UniqueKeys).

hb_move(H, P, M, Residue) :-
    H =.. [R|List],
    hb_list_move(List, P, SubM, SubResidue),
    attach(SubM, SubResidue, NewList, Residue),
    M =.. [R|NewList].

hb_list_move([H|T], P, M, Residue) :-
    hb_move(H, P, SubM, Residue),
    ((SubM = end) -> (M = T) ; M = [SubM|T]).

hb_list_move([H|T], P, M, Residue) :-
    hb_list_move(T, P, SubM, Residue),
    M = [H|SubM].

get_keys(Num, [Num]) :-
    number(Num).
get_keys(Branch, Keys) :-
    \+number(Branch),
    Branch =.. [_|Parts],
    get_list_keys(Parts, Keys).

get_list_keys([], []).
get_list_keys([H|T], Keys) :-
    get_keys(H, Int1),
    get_list_keys(T, Int2),
    append(Int1, Int2, Keys).

attach([], R, [], R) :- !.
attach(L, residue(B, []), L, residue(B, [])) :- !.
attach([H|T], residue(Branch, Keys), NewL, residue(end, [])) :-
    member(H, Keys),
    attach_branch(Branch, H, [], NewBranches),
    append(T, NewBranches, NewL).
attach([H|T], Residue, [NewH|NewT], NewResidue) :-
    Residue = residue(_, Keys),
    \+member(H, Keys),
    H =.. [Branch|HList],
    attach(HList, Residue, NewHList, IntResidue),
    NewH =.. [Branch|NewHList],
    attach(T, IntResidue, NewT, NewResidue).

attach_branch(Branch, Key, Parents, NewBranches) :-
    Branch =.. [_|Parts],
    member(Key, Parts),
    select(Key, Parts, Siblings),
    append(Siblings, Parents, NewBranches), !.
attach_branch(Branch, Key, Parents, NewBranches) :-
    Branch =.. [_|Parts],
    select(Choice, Parts, Others),
    append(Others, Parents, AllOthers),
    Choice =.. [Head|_],
    atom(Head),
    NextParents =.. [Head|AllOthers],
    attach_branch(Choice, Key, [NextParents], NewBranches).
