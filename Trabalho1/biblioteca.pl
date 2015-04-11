/* ######################### Biblioteca de funções ##########################*/
/* Verifica o comprimento de uma lista e imprime os passos intermediários
 * da contagem na tela em uma nova linha cada. 
 * */
privado_ComprimentoDaLista([],0).
privado_ComprimentoDaLista([_|L],N) :- 
	privado_ComprimentoDaLista(L,N1), 
	N is N1 + 1, 
	write(N), 
	writef('\n').

/* Dada uma posição K, dadoDeInfoNaPosicao(Posicao, dadoDeRetorno), 
 * retorna um elemento em uma dada posição na lista definida no predicado 
 * informacoesPessoais([...]) 
 * */
privado_DadoNaPosicao(X, [X|_], 0).
privado_DadoNaPosicao(X,[_|L],K) :- 
	K > 0, 
	K1 is K - 1, 
	privado_DadoNaPosicao(X,L,K1).
	
dadoDeInfoNaPosicao(Posicao, DadoDeRetorno) :- 
	informacoesPessoais(L), 
	privado_DadoNaPosicao(DadoDeRetorno, [_|L], Posicao).

/* Dado uma lista numérica, retora a soma de seus elementos na variável Soma 
 * */
somaDosElementos(Lista, Soma) :- 
	somaDosElementos(Lista, 0, Soma).
somaDosElementos([], Soma, Soma).
somaDosElementos([CabecaDaLista|Lista], Soma0, Soma) :-
	Soma1 is Soma0 + CabecaDaLista,
	somaDosElementos(Lista, Soma1, Soma).

/*
 * Modelo de uso de variáveis globais.
 * */
recursao(100).
recursao(X) :- 
	adicionar, 
	Y is X + 1, 
	recursao(Y).

adicionar :- 
	nb_getval(contador, C), 
	NovoC is C + 1, 
	nb_setval(contador, NovoC).

testarRecursao :-
    % Ajusta o contador para 10
    nb_setval(contador, 10),

    % Executa algumas somas
    recursao(50), !,

    % Imprime os resultados
    nb_getval(contador, ContadorValor),
    write('Recurcoes: '), 
    writeln(ContadorValor).


/* Recebe os parametros, privado_DividirLista(L,N,L1,L2). 
 * Uma lista L e cria duas listas, L1 e L2, onde a primeira contém N elementos 
 * e a segunda o restante.
 * */
privado_DividirLista(L,0,[],L).
privado_DividirLista([X|Xs],N,[X|Ys],Zs) :- 
	N > 0, 
	N1 is N - 1, 
	privado_DividirLista(Xs,N1,Ys,Zs).

/* Verifica se uma element P é membro da cabeça da lista. Faz isso para garantir 
 * que não seja retornado verdadeiro caso P seja encontrado no final da lista.
 * */
privado_Is_Head_Member(P, L) :- 
	L = [P|_].








