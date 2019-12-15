%% ?- consult('utilities.pl').

memb(H, [H|_]).
memb(H, [_|T]) :- memb(H, T).

app([], L, L).
app([H|T], L, [H|U]) :- app(T, L, U).
