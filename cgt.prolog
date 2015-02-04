:- module(cgt, [move/3, winner/2, game_label/2]).
:- multifile move/3, winner/2, game_label/2.

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

cgt:distinct_move(G, P, G2) :-
    setof(G1, cgt:move(G, P, G1), Moves),
    member(G2, Moves).

enemy(red, blue).
enemy(blue, red).

% ANALYSIS

:- dynamic analysis/3.

cgt:analyse_game(G, P, A) :-
    analysis(G, P, A).
cgt:analyse_game(G, P, A) :-
    \+analysis(G, P, _),
    once(raw_analyse(G, P, A1)),
    assertz(analysis(G, P, A1)),
    A = A1.

raw_analyse(Game, Player, Winner) :-
    enemy(Player, Enemy),
    findall(M, cgt:move(Game, Player, M), Moves),
    once(raw_analyse_moves(Game, Player, Enemy, Moves, Winner)).

raw_analyse_moves(Game, Player, Enemy, [], Winner) :-
    cgt:winner(Game/Player, Winner).
raw_analyse_moves(Game, Player, Enemy, [], black) :-
    \+cgt:winner(Game/Player, _).
raw_analyse_moves(Game, Player, Enemy, Moves, Player) :-
    once((member(Move, Moves), cgt:analyse_game(Move, Enemy, Player))).
raw_analyse_moves(Game, Player, Enemy, Moves, Enemy) :-
    forall(member(Move, Moves), cgt:analyse_game(Move, Enemy, Enemy)).
raw_analyse_moves(Game, Player, Enemy, Moves, black).


print_analysis :-
    setof([G,P,V], cgt:analysis(G,P,V), L),
    member(X, L), format('~w/~w -> ~w~n', X), fail.

reset_analysis :-
    retractall(analysis(_,_,_)).

descendant(Game/Player, Game/Player).
descendant(Game/Player, DescGame/DescPlayer) :-
    children(Game/Player, Children),
    member(Game2/Player2, Children),
    descendant(Game2/Player2, DescGame/DescPlayer).

descendants(Game/Player, Descs) :-
    setof(DescGame/DescPlayer, descendant(Game/Player, DescGame/DescPlayer), Descs).

children(Game/Player, Children) :-
    enemy(Player, Player2),
    once((setof(DescGame/Player2, move(Game, Player, DescGame), Children);
        Children = [])).

% GRAPHIC OUTPUT

game_key(Game/Player, Key) :-
    format(codes(Codes), '~w', Game/Player),
    game_key_symbols(Codes, NewCodes),
    string_codes(Key, NewCodes).

game_key_symbols([], []).
game_key_symbols([H|T], [H2|T2]) :-
    once(game_key_symbol(H, H2)),
    game_key_symbols(T, T2).

game_key_symbol(32, 95).
game_key_symbol(40, 80).
game_key_symbol(44, 67).
game_key_symbol(41, 81).
game_key_symbol(47, 83).
game_key_symbol(91, 66).
game_key_symbol(45, 95).
game_key_symbol(93, 68).
game_key_symbol(X, X).

get_game_label(Game, Label) :-
    once((game_label(Game, Label);
        format(string(Label), '~w', [Game]))).

dot(Game, Player) :-
    format('digraph {~n'),
    dot_nodes(Game/Player);
    format('}~n').

dot_nodes(Game/Player) :-
    descendants(Game/Player, Descs),
    member(G/P, Descs),
    children(G/P, Children),
    analyse_game(G, P, Value),
    game_key(G/P, Key),
    get_game_label(G, Label),
    dot_player_suffix(Children, P, PlayerSuffix),
    format('~w [label=<~w~w>,color=~w];~n', [Key, Label, PlayerSuffix, Value]),
    member(G2/P2, Children),
    game_key(G2/P2, Key2),
    format('~w -> ~w;~n', [Key, Key2]),
    fail.

dot_player_suffix([], Player, Suffix) :-
    atom_string('', Suffix).
dot_player_suffix([_|_], Player, Suffix) :-
    format(string(Suffix), '<BR/><FONT COLOR="~w">~w</FONT>', [Player, Player]).
