:- module(xox, []).

cgt:move(N, P, N2) :-
    N =.. [xox|H],
    cgt:enemy(P, Enemy),
    \+cgt:winner(N/P, Enemy),
    player_symbol(P, PS),
    xox_move(H, PS, H2),
    xox_normalise(H2, H2Normalised),
    N2 =.. [xox|H2Normalised].

cgt:winner(Game/_Player, Winner) :-
    Game =.. [xox|M],
    player_symbol(Winner, Symbol),
    once((line(3, M, L), sort(L, [Symbol]))).

cgt:game_label(G, L) :-
    G =.. [xox|Rows],
    findall(RL, (member(R, Rows), row_label(R, RL)), RLs),
    atom_string('<BR/>', BR),
    string_join(BR, RLs, L).

player_symbol(red, x).
player_symbol(blue, o).

row_label(R, RL) :-
    findall(L, (member(X, R), atom_string(X, L)), Ls),
    atom_string(' ', SP),
    string_join(SP, Ls, RL).

test_state(0, xox([-,-,-], [-,-,-], [-,-,-])).
test_state(1, xox([x,x,-], [-,o,-], [o,-,-])).
test_state(2, xox([x,x,o], [-,o,-], [-,o,x])).
test_state(3, xox([x,x,x], [-,o,-], [o,-,-])).
test_state(4, xox([x,x,o], [o,o,x], [x,o,x])).

xox_move([H|T], P, [H2|T]) :-
    xox_row_move(H, P, H2).
xox_move([H|T], P, [H|T2]) :-
    xox_move(T, P, T2).

xox_row_move([-|T], P, [P|T]).
xox_row_move([H|T], P, [H|T2]) :-
    xox_row_move(T, P, T2).

xox_normalise(M, NormalisedM) :-
    setof(M2, symmetry(M, M2), Ms),
    reverse(Ms, [NormalisedM|_]).

symmetry(M, M2) :- symmetry2(M, Int), (M2 = Int; flipV(Int, M2)).
symmetry2(M, M2) :- symmetry3(M, Int), (M2 = Int; flipH(Int, M2)).
symmetry3(M, M2) :- M2 = M; flipT(M, M2).

flipV(M, M2) :-
    reverse(M, M2).
flipH(M, M2) :-
    findall(R2, (member(R, M), reverse(R, R2)), M2).
flipT(M, M2) :-
    M = [F|_],
    length(F, Width),
    findall(R2,
        (between(1, Width, N),
        findall(X, (member(R, M), nth1(N, R, X)), R2)),
        M2).

line(Size, M, L) :-
    Max is Size - 1,
    line_def(Size, M, X,Y,XStep,YStep),
    findall(V, (between(0, Max, P), PX is X + XStep * P, PY is Y + YStep * P, nth0(PY, M, R), nth0(PX, R, V)), L).

line_def(Size, M, X,Y,1,0) :-
    dims(M, Width, Height),
    MaxX is Width - Size,
    MaxY is Height - 1,
    between(0, MaxX, X),
    between(0, MaxY, Y).
line_def(Size, M, X,Y,0,1) :-
    dims(M, Width, Height),
    MaxX is Width - 1,
    MaxY is Height - Size,
    between(0, MaxX, X),
    between(0, MaxY, Y).
line_def(Size, M, X,Y,1,1) :-
    dims(M, Width, Height),
    MaxX is Width - Size,
    MaxY is Height - Size,
    between(0, MaxX, X),
    between(0, MaxY, Y).
line_def(Size, M, X,Y,-1,1) :-
    dims(M, Width, Height),
    MinX is Size - 1,
    MaxX is Width - 1,
    MaxY is Height - Size,
    between(MinX, MaxX, X),
    between(0, MaxY, Y).

dims(M, Width, Height) :-
    length(M, Height),
    M = [F|_],
    length(F, Width).
