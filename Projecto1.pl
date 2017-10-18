import :- import('paises.txt').
import(TXT) :- [TXT].

%-------------------

lista(C):-
    write('Continente: '), write(C), nl,
    pais(P,C,H), write(P), write(, ), write(H), write(, ),
    paisesFronteira(P,_,Lst), write(Lst).

paisesFronteira(P,X,Lst) :-
    findall(X, fronteira(P,X); fronteira(X,P), Lst).

%-------------------

doisMaisPop(P1, P2) :- 
    findall(H, pais(_,_,H), LstH),
    sort(0, @>=, LstH,  SortedH),
    doisMaisPop(P1, P2, SortedH).

doisMaisPop(P1, P2, [H1|[H2|_]]) :-
    pais(P1, _, H1),
    pais(P2, _, H2).

%------------------

paisesGrandes(C, N, L) :- 
    findall(H-P, (pais(P,C,H), H > N), L).

%------------------

somaPopViz(P, L, S) :-
    findall((H, P2), ((fronteira(P, P2); fronteira(P2, P)), pais(P2,_,H)), L), write(L),
    somaVizinhos(L, S).

somaVizinhos([],0).
somaVizinhos([(N,_)|L],S):-
    somaVizinhos(L,S1),
    S is S1 + N.

%-------------------

