color(blue).
color(red).
color(yellow).
color(orange).
color(green).
color(white).
color(black).

%-utils--------------

connected2(A, B) :- fronteira(A, B) ; fronteira(B, A).

%-1------------------

import :- import_file('paises.txt').
import_file(TXT) :- [TXT].

%-2------------------

lista(C):-
    write('Continente: '), write(C), nl,
    pais(P, C, H), write(P), write(, ), write(H), write(, ),
    paisesFronteira(P, _, Lst), write(Lst).

paisesFronteira(P,X,Lst) :-
    findall(X, connected2(P, X), Lst).

%-3------------------

doisMaisPop(P1, P2) :- 
    findall(H, pais(_, _, H), LstH),
    sort(0, @>=, LstH,  SortedH),
    doisMaisPop(P1, P2, SortedH).

doisMaisPop(P1, P2, [H1|[H2|_]]) :-
    pais(P1, _, H1),
    pais(P2, _, H2).

%-4-----------------

paisesGrandes(C, N, L) :- 
    findall(H-P, (pais(P, C, H), H > N), L).

%-5-----------------

somaPopViz(P, L, S) :-
    findall((H, P2), (connected2(P, P2), pais(P2, _, H)), L), write(L),
    somaVizinhos(L, S).

somaVizinhos([], 0).
somaVizinhos([(N,_)|L], S):-
    somaVizinhos(L, S1),
    S is S1 + N.

%-6------------------

numPaisesAtravessados(P1, P2, Num):- bfs(P1, P2, Cam), 
    length(Cam, X),
    Num is X - 1.

bfs(Orig, Dest, Cam):- bfs2(Dest, [[Orig]], Cam).

bfs2(Dest, [[Dest|T]|_], Cam):- reverse([Dest|T], Cam).

bfs2(Dest, [LA|Outros], Cam):-
    LA=[Act|_],
    findall([X|LA], (Dest\==Act, 
        (connected2(Act, X)), 
        \+ member(X, LA)),
        Novos),
    append(Outros, Novos, Todos),
    bfs2(Dest, Todos, Cam).

%-7------------------

route(Orig, Dest, NFront, Cam):- 
    calcRoute(Orig, Dest, NFront, Cam),
    NFront1 is NFront + 1,
    length(Cam, NFront1).

calcRoute(Orig, Dest, NFront, Cam):- 
    calcRoute2(Dest, [[Orig]], NFront, Cam,_).

calcRoute2(Dest, [[Dest|T]|_], _, Cam, _):- 
    reverse([Dest|T], Cam).

calcRoute2(Dest, [LA|Outros], NFront, Cam, Num):-
    LA = [Act|_],
    findall([X|LA],
        (Dest\==Act, connected2(Act, X),
        \+ member(X, LA), length(LA, N),
        N =< NFront, Num=N),
        Novos),
    append(Outros, Novos, Todos),
    calcRoute2(Dest, Todos, NFront, Cam, Num).

%-8------------------

roteiroContinente(Cont, Orig, NFront, Cam):- 
    pais(Orig, Cont, _), 
    calculaRoteiros(Orig, _, NFront, Cam), 
    NFront1 is NFront + 1, 
    length(Cam, NFront1).

calculaRoteiros(Orig, Dest, NFront, Cam):- 
    calculaRoteiros2(Dest, [[Orig]], NFront, Cam).

calculaRoteiros2(Dest, [[Dest|T]|_], _, Cam):- 
    reverse([Dest|T], Cam).

calculaRoteiros2(Dest, [LA|Outros], NFront, Cam):-
    LA=[Act|_],
    findall([X|LA],
        (Dest\==Act, 
        connected2(Act,X), 
        \+ member(X,LA), 
        length(LA,N), 
        N =< NFront),
        Novos),
    append(Outros, Novos, Todos),
    calculaRoteiros2(Dest, Todos, NFront, Cam).
    
%-9------------------

paisesContinente(C,L):- 
    findall(P, pais(P,C,_),L).

getColors(Col):- 
    findall(C, color(C),Col).

conflicts(P1,Color,[[P2,Color]|_]):- 
    fronteira(P1,P2);fronteira(P2,P1).

conflicts(P,Color,[_|Coloring]):- %
    conflicts(P,Color,Coloring).

colorir([], _, []).

colorir([H|T],Col,[[H,FColor]|Rest]):-
    colorir(T,Col,Rest),
    member(FColor,Col),
    \+conflicts(H,FColor,Rest).

colocaBC([]).
colocaBC([[H|T]|Rest]):-
    colocaBC(Rest),
    asserta(cor(T,H)), write(T), write(H), nl.

colorir_mapa(C):- 
    paisesContinente(C,L), 
    getColors(Col), 
    colorir(L,Col,FL), 
    colocaBC(FL).

%-10-----------------

checkCores(R):- 
    findall((P, C, L), (cor(C, P), listaCorViz(P, L)), R).

listaCorViz(P, L):- 
    findall(C, (connected2(P,P2), cor(C, P2)), L).

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
    
   forall(color(X),
          (write(OS,'color('),
           write(OS,X),
           write(OS,').'), nl(OS))
         ),
    nl(OS),

    forall(cor(X,Y),
          (write(OS,'cor('),
           write(OS,X), write(OS,', '),
           write(OS,Y),
           write(OS,').'), nl(OS))
         ), 

   close(OS),
   write('Done!').