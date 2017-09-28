
media([],0).
media(L,M):- mediaint(L,C,S), M is S/C.
mediaint([],0,0).
mediaint([H|T],C,S):- mediaint(T,C1,S1), S is S1 + H,  C is C1 + 1.

menorlst([H],H).
menorlst([H|T],M):- menorlst(T,M1), menor(H,M1,M).
menor(A,B,A):- B>A, !.
menor(_,B,B). 

contarParImp([], 0, 0).
contarParImp([H|T],P,I):- contarParImp(T, P1, I1), par(H,C), impar(H,D), P is P1 + C, I is I1 + D.
par(N,C):- R is mod(N,2), R == 0, C is 1; C is 0.
impar(N,C):- R is mod(N,2), R \== 0, C is 1; C is 0.

/* reverse true false */
reptlst([]):- true. 
reptlst([H|T]):- rept(H,T), reptlst(T).
rept(_,[]).
rept(A,[H|T]):- write(A), write(H), write(T), nl, A \== H, rept(A,T).





