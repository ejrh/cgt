string_join(_, [], E) :-
    string_codes(E, []).
string_join(_, [S], S).
string_join(J, [H,H2|T], S) :-
    string_concat(H, J, Int),
    string_join(J, [H2|T], Int2),
    string_concat(Int, Int2, S).
