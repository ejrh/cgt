:- module(game, []).
:- multifile(move/3).

test_state(0, H) :-
    H = game([], []).

test_state(1, H) :-
    H = game([game([], [])], [game([], [])]).

cgt:move(game(Left, Right), P, NewGame) :-
    game_move(game(Left, Right), P, NewGame).

game_move(game(Left, _Right), blue, NewGame) :-
    member(NewGame, Left).

game_move(game(_Left, Right), red, NewGame) :-
    member(NewGame, Right).

convert(Game, game(Left, Right)) :-
    findall(convert(SubGame), move(Game, blue, SubGame), Left),
    findall(convert(SubGame), move(Game, red, SubGame), Right).
