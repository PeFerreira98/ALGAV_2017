continente(europa).
continente(africa).
continente(america).
continente(asia).
continente(oceania).

pais(portugal, europa, 10).
pais(espanha, europa, 20).
pais(franca, europa, 30).
pais(alemanha, europa, 50).
pais(canada, america, 50).
pais(us, america, 110).
pais(brazil, america, 100).
pais(madeira, europa, 1).

fronteira(portugal, espanha).
fronteira(espanha, franca).
fronteira(canada, us).
fronteira(us, brazil).
fronteira(alemanha, franca).
fronteira(alemanha, espanha).

vizinho(X,Y):- fronteira(X,Y); fronteira(Y,X).

contSemPaises(C):- continente(C), \+ pais(_,C,_).

semVizinhos(X):- pais(X,_,_), \+ vizinho(X,_).

chegoLaFacil(X,Y):- vizinho(X,Y); vizinho(X,Z), vizinho(Z,Y), X\=Y.

factorial(N,_):- N<0, write('Numero invalido!'), nl.
factorial(0,1).
factorial(N,F):- N>0, X is N-1, factorial(X,R), F is N*R.

potencia(_,0,1).
potencia(X,Y,Z):- Y>0, Y2 is Y-1,  potencia(X, Y2, Z2), Z is X*Z2.
potencia(X,Y,Z):- Y<0, Y2 is (Y* -1),  potencia(X, Y2, Z2), Z is (1/Z2).

somatorio(J,K,Z):- J=K, Z is K.   
somatorio(J,K,Z):- J<K, J2 is J+1, somatorio(J2, K, Z2), Z is J+Z2.
somatorio(J,K,Z):- J>K, J2 is J-1, somatorio(J2, K, Z2), Z is J+Z2.

divid(X,Y,Q,R):- X=Y, Q is 1, R is 0.
divid(X,Y,Q,R):- X<Y, Q is 0, R is Y-X.
divid(X,Y,Q,R):- X>Y, X2 is X-Y, divid(X2,Y,Q2,R), Q is Q2+1.

/* primo(2). */

/* mdc(A,B,Z):- A>B, divid(A,B,Q,R), R==0, Z=Q). */