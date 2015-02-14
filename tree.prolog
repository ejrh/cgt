:- module(tree, []).

% Randomly assign edge labels of tree.
random_edges(T, Labels, NewT) :-
    T =.. [_|List],
    random_member(NewE, Labels),
    findall(T2, (member(T1, List), random_edges(T1, Labels, T2)), NewList),
    NewT =.. [NewE|NewList].

% Generate all trees on N nodes.
generate_tree(N, T) :-
    M is N - 1,
    generate_tree_list(M, L),
    T =.. [tree|L].

generate_tree_list(0, []).
generate_tree_list(N, [H|T]) :-
    between(1, N, M1),
    M2 is N - M1,
    generate_tree(M1, H),
    generate_tree_list(M2, T).

% Create a random tree on N nodes with uniform probability.
random_tree(N, T) :-
    M is N - 1,
    random_tree_list(M, L),
    T =.. [tree|L].

random_tree_list(0, []).
random_tree_list(N, [H|T]) :-
    %random_between(1, N, M1),
    random_catalan_size(N, M1),
    M2 is N - M1,
    random_tree(M1, H),
    random_tree_list(M2, T).

catalan_number(0,1).
catalan_number(1,1).
catalan_number(2,2).
catalan_number(3,5).
catalan_number(4,14).
catalan_number(5,42).
catalan_number(6,132).
catalan_number(N, C) :-
    N > 6,
    Nm1 is N - 1,
    catalan_number(Nm1, CNm1),
    Factor is 2 * (2 * Nm1 + 1) / (Nm1 + 2),
    C is round(Factor * CNm1).

catalan_distribution(I, J, R, X) :-
    R >= 1,
    catalan_number(I, C1),
    catalan_number(J, C2),
    Product is C1 * C2,
    ((R =< Product) -> X is I + 1;
    NewI is I + 1,
    NewJ is J - 1,
    NewR is R - Product,
    catalan_distribution(NewI,NewJ,NewR,X)).

random_catalan_size(1, 1).
random_catalan_size(2, X) :-
    random_between(1, 2, X).
random_catalan_size(N, X) :-
    N >= 3,
    catalan_number(N, C),
    random_between(1, C, R),
    N1 is N - 1,
    catalan_distribution(0, N1, R, X).

% Create a random binary tree on N nodes, WITHOUT uniform probability.
random_binary_tree(0, empty).
random_binary_tree(N, tree(L, R)) :-
    N1 is N - 1,
    random_between(0, N1, NL),
    NR is N1 - NL,
    random_binary_tree(NL, L),
    random_binary_tree(NR, R).
