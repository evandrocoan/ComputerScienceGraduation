/* ######################### Biblioteca de funções ##########################*/
/* Predicado: primeiro(L,P)
 * Definição: L é uma lista e P é o primeiro dado de L.
 * */
primeiro([P|_],P).


/* Predicado: ultimo(L,U)
 * Definição: L é uma lista e U é o último dado de L.
 * */
ultimo( [X], X ).
ultimo( [_ | Y], X ) :- ultimo( Y, X ).


/* Predicado: pertence(D,L)
 * Definição: L é uma lista e D é um dos dados de L
 * */
pertence( D, [D|_] ).
pertence( D, [_|L] ) :- pertence( D, L ).


/* Predicado: inseridoNoFinal(D, L, LM)
 * Definição: D é um dado, L é uma lista e LM é a lista L mais o dado
 * D no final.
 * */
inseridoNoFinal( D, [], [D] ).
inseridoNoFinal( D, [X | L], [X | LM] ) :- inseridoNoFinal( D, L, LM ).


/* Predicado: substituidoDoInicio(D, L, LM)
 * Definição: D é um dado, L é uma lista e LM é a lista L com
 * D no lugar do primeiro dado de L.
 * */
substituidoDoInicio( D, [_ | L], [D | L] ).


/* Predicado: substituidoDaPos(D, Pos, L, LM)
 * Definição: D é um dado, Pos é a posição do dado a ser substituído,
 * L é uma lista e LM é a lista L com D no lugar do dado de L que está
 * na posição Pos.
 * */
substituidoDaPos( D, 0, [_ | L], [D | L] ).
substituidoDaPos( D, Pos, [K|L], [K|LM] ) :-
    substituidoDaPos( D, MPos, L, LM ), Pos is MPos + 1.


/* Predicado: invertida(L, LM)
 * Definição: L é uma lista e LM é a lista L com os dados
 * invertidos (o primeriro de L será o último de LM,
 * o segundo de L será o penúltimo de LM e assim por diante).
 * */
invertida( [], [] ).
invertida( [D | L], LI ) :- mesmoTamanho( L, LM ),
    inseridoNoFinal( D, LM, LI ), !,
    invertida( LM, L ).

/* Este predicado-gambiarra cria uma lista de variáveis do mesmo
 * tamanho de uma lista constante, se a outra for deixada variável.
 * */
mesmoTamanho( [], [] ).
mesmoTamanho( [_|L], [_|M] ) :- mesmoTamanho( L, M ).


/* Predicado: temOcorrencias(D,L,O)
 * Definição: D é um dado, L é uma lista e O é um número que indica
 * quantas vezes D aparece na lista L.
 * */
temOcorrencias( D, L, O ) :- dataset( L, Data ),
            pertence( p( D, O ), Data ).
temOcorrencias( D, L, 0 ) :- dataset( L, Data ),
            \+pertence( p(D, _), Data ).


/* datainsert( D, Data, Idata )
 * Definição: Data é uma lista de pares (X, N), em que X é um elemento
 * e N é um número, e Idata é esta lista de pares, com o par (D, N)
 * com o N incrementado em um.
 * */
datainsert( D, [], [p(D, 1)] ).
datainsert( D, [p(D, N) | L], [p(D, M) | L] ) :- !, M is N + 1.
datainsert( D, [K | Data], [K | Idata] ) :-
            datainsert( D, Data, Idata ).


/* dataset( L, Data )
 * Definição: L é uma lista de dados, Data é uma lista de pares (X, N),
 * em que X é um elemento de L e N é sua quantidade de aparições em L.
 * */
dataset( [], [] ).
dataset( [D | L], Idata ) :- dataset( L, Data ),
            datainsert( D, Data, Idata ).


/* Predicado: semOcorrencias(D,L,LM)
 * Definição: D é um dado, L é uma lista e LM é a lista L sem a
 * presença do dado D. A ordem dos dados em LM tem que ser a
 * mesma de L.
 * */
semOcorrencias( _, [], [] ).
semOcorrencias( D, [D | L], LM ) :- !, semOcorrencias( D, L, LM ).
semOcorrencias( D, [K | L], [K | LM] ) :- semOcorrencias( D, L, LM ).


/* Predicado: concatenadas(L1, L2, LC)
 * Definição: L1 e L2 são listas e LC é a lista contendo todos os dados
 * de L1 seguidos de todos os dados de L2.
 * */
concatenadas( [], L, L ).
concatenadas( [D | L1], L2, [D | LC] ) :- concatenadas( L1, L2, LC ).


/* Predicado: ehMaior(L,M)
 * Definição: L é uma lista de números e M é o maior dos números.
 * */
ehMaior( [M], M ).
ehMaior( [X | L], M ) :- ehMaior( L, Y ), max( X, Y, M ).


/* Predicado auxiliar: max( X, Y, M )
 * Definição: M é o máximo entre X e Y. 
 * */
max( X, Y, X ) :- X > Y.
max( X, Y, Y ) :- Y > X.
max( X, X, X ).


/* Predicado: primeiroEUltimo(L, LPU)
 * Definição: L é uma lista e LPU é uma lista contendo o primeiro
 * e o último dados de L.
 * */
primeiroEUltimo( L, [P, U] ) :- primeiro( L, P ), ultimo( L, U ).


/* Predicado: nivelada(L,LN)
 * Definição: L é uma lista que pode conter outras listas e LN é uma
 * lista formada pelos dados de L com uma diferença: quando um dado de
 * L for uma lista então os dados desta lista são incluídos na lista
 * LN.
 *
 * Exemplo: ? nivelada([a,b, [c, d], e], LN).
 * LN = [a, b, c, d, e]
 * */
nivelada( [], [] ).
nivelada( [E | L], C ) :- 
    ehLista( E ), 
    !, 
    nivelada( L, LN ),
    nivelada( E, EN ), 
    concatenadas( EN, LN, C ).

nivelada( [D | L], [D | LN] ) :- 
    nivelada( L, LN ).


/* Predicado auxiliar: ehLista( L )
 * Verdadeiro apenas para listas.
 * */
ehLista( [] ).
ehLista( [_ | L] ) :- ehLista( L ).


/* Predicado: invertida2(L,LI)
 * Definição: L é uma lista e LI é uma lista formada pelos membros de
 * L só que na ordem inversa. A implementação não deve usar o predicado 
 * concatenadas.
 * */
% O predicado invertida( L, LI ) atende às especificações.


/* Predicado: impares(L, LI)
 * Definição: L é uma lista e LI é uma lista que contém os dados de L
 * nas posições ímpares.
 * */
impares( [], [] ).
impares( [_ | L], LN ) :- pares( L, LN ).


/* Predicado auxiliar: pares( L, LP )
 * LP contém os dados de L nas posições pares. 
 * */
pares( [], [] ).
pares( [D | L], [D | LN] ) :- impares( L, LN ).


/* Predicado: maioresQue(L1, Limite, L2)
 * Definição: L1 é uma lista de números, L2 é uma lista que contém
 * dados de L1 que são maiores que Limite.
 * */
maioresQue( [], _, [] ).
maioresQue( [D | L1], Limite, [D | L2] ) :- D > Limite, !,
            maioresQue( L1, Limite, L2 ).
maioresQue( [_ | L1], Limite, L2 ) :- maioresQue( L1, Limite, L2 ).


/* Predicado: monteLista(D, C, L)
 * Definição: L é uma lista formada por C dados iguais a D.
 * */
monteLista( _, 0, [] ) :- !.
monteLista( D, M, [D | L] ) :- N is M-1, monteLista( D, N, L ).


/* Verifica o comprimento de uma lista e imprime os passos intermediários
 * da contagem na tela em uma nova linha cada. 
 * */
comprimentoDaLista([],0).
comprimentoDaLista([_|L],N) :- 
	comprimentoDaLista(L,N1), 
	N is N1 + 1, 
	write(N), 
	writef('\n').


/* Dada uma posição K, dadoDeInfoNaPosicao(Posicao, dadoDeRetorno), 
 * retorna um elemento em uma dada posição na lista definida no predicado 
 * informacoesPessoais([...]) 
 * */
dadoDeInfoNaPosicao(Posicao, DadoDeRetorno) :- 
    informacoesPessoais(L), 
    dadoNaPosicao(DadoDeRetorno, [_|L], Posicao).


/* dadoNaPosicao(Dado, Lista, Posicao).
 * Dada uma posição Posicao retorna um elemento Dado em uma dada posição na 
 * lista Lista.
 * */
dadoNaPosicao(X, [X|_], 0).
dadoNaPosicao(X,[_|L],K) :- 
	K > 0, 
	K1 is K - 1, 
	dadoNaPosicao(X,L,K1).


/* Dado uma lista numérica, retora a soma de seus elementos na variável Soma. 
 * */
somaDosElementos(Lista, Soma) :- 
	somaDosElementos(Lista, 0, Soma).
somaDosElementos([], Soma, Soma).
somaDosElementos([CabecaDaLista|Lista], Soma0, Soma) :-
	Soma1 is Soma0 + CabecaDaLista,
	somaDosElementos(Lista, Soma1, Soma).


/* Recebe os parametros, dividirLista(L,N,L1,L2). 
 * Uma lista L e cria duas listas, L1 e L2, onde a primeira contém N elementos 
 * e a segunda o restante.
 * */
dividirLista(L,0,[],L).
dividirLista([X|Xs],N,[X|Ys],Zs) :- 
	N > 0, 
	N1 is N - 1, 
	dividirLista(Xs,N1,Ys,Zs).


/* Verifica se uma element P é membro da cabeça da lista. Faz isso para garantir 
 * que não seja retornado verdadeiro caso P seja encontrado no final da lista.
 * */
isHeadMember(P, L) :- 
	L = [P|_].

/* maiorElemento( Lista, Maior )
 * Dado um Lista retorna o Maior elemento dela.
 * */
maiorElemento([X],X).
maiorElemento([X|Xs],X) :- max(Xs,Y), X >= Y.
maiorElemento([X|Xs],N) :- max(Xs,N), N > X.





