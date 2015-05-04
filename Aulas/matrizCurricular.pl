disciplina(poo1).
disciplina(poo2).

nome(poo1, 'Programação Orientada à Objetos I').
nome(poo1, 'Programação Orientada à Objetos II').

fase1(poo1).
fase1(circuitos).
fase1(fmd).
fase1(calcA).
fase1(intro).

fase2(sd).
fase2(poo2).
fase2(calcB).
fase2(prob).
fase2(ga).
fase2(cts).

fase3(ed).

prerequisito(poo1, poo2).
prerequisito(grafos, ia).
prerequisito(paradigmas, ia).

prerequisito(poo2, ed).

preprereq(X, Z) :- prerequisito(X, Y), prerequisito(Y, Z).