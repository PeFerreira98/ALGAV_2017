%-1------------------

import :- import('paises.txt').
import(TXT) :- [TXT].

%-2------------------

lista(C):-
    write('Continente: '), write(C), nl,
    pais(P,C,H), write(P), write(, ), write(H), write(, ),
    paisesFronteira(P,_,Lst), write(Lst).

paisesFronteira(P,X,Lst) :-
    findall(X, fronteira(P,X); fronteira(X,P), Lst).

%-3------------------

doisMaisPop(P1, P2) :- 
    findall(H, pais(_,_,H), LstH),
    sort(0, @>=, LstH,  SortedH),
    doisMaisPop(P1, P2, SortedH).

doisMaisPop(P1, P2, [H1|[H2|_]]) :-
    pais(P1, _, H1),
    pais(P2, _, H2).

%-4-----------------

paisesGrandes(C, N, L) :- 
    findall(H-P, (pais(P,C,H), H > N), L).

%-5-----------------

somaPopViz(P, L, S) :-
    findall((H, P2), ((fronteira(P, P2); fronteira(P2, P)), pais(P2,_,H)), L), write(L),
    somaVizinhos(L, S).

somaVizinhos([],0).
somaVizinhos([(N,_)|L],S):-
    somaVizinhos(L,S1),
    S is S1 + N.

%-6------------------

numPaisesAtravessados(P1,P2,Num):- bfs(P1,P2,Cam), 
    length(Cam,X),
    Num is X-1.

bfs(Orig,Dest,Cam):- bfs2(Dest, [[Orig]], Cam).

bfs2(Dest,[[Dest|T]|_], Cam):- reverse([Dest|T], Cam).

bfs2(Dest,[LA|Outros],Cam):-
    LA=[Act|_],
    findall([X|LA], (Dest\==Act, 
        (fronteira(Act,X);fronteira(X,Act)), 
        \+ member(X,LA)),
        Novos),
    append(Outros,Novos,Todos),
    bfs2(Dest,Todos,Cam).


%-11-----------------

exportar :-
    exportar('test.txt').

exportar(F) :-
   write('Exporting...'), nl,
   open(F,write,OS),

   forall(continente(X),
          (write(OS,'continente('), write(OS,X),
           write(OS,').'), nl(OS))
         ),
   nl(OS),

   forall(pais(X,Y,Z),
           (write(OS,'pais('), write(OS,X), write(OS,', '),
            write(OS,Y), write(OS,', '),
            write(OS,Z), write(OS,').'), nl(OS))
           ),
   nl(OS),

   forall(fronteira(X,Y),
          (write(OS,'fronteira('), write(OS,X), write(OS,', '),
           write(OS,Y), write(OS,').'), nl(OS))
         ),

   close(OS),
   write('Done!').

