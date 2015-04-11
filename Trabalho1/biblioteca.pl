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

	