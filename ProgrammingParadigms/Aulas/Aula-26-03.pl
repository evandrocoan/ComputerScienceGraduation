homem(alberto).
homem(eduardo).
mulher(alice).
mulher(vitoria).
pais(eduardo, vitoria, alberto).
pais(alice, vitoria, alberto).

irma_de(X, Y) :- mulher(X), pais(X, M, P), pais(Y, M, P), dif(X,Y).

dif(X,Y) :- X \== Y.
