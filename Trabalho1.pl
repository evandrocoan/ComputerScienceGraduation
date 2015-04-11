:- use_module(library(date)).

/* Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 * Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
formacaoNoCursoDe( ['Pessoa9 da Silva1', 'Ciencias da Computacao', 
'ITA', 'Silva1 da Pessoa9', 2010, 2015 ] ).
formacaoNoCursoDe( ['Pessoa9 da Silva2', 'Ciencias da Computacao', 
'ITA', 'Silva2 da Pessoa8', 2003, 2007, 'Avril Lavigne', 'Demi Lovato' ] ).
formacaoNoCursoDe( ['Pessoa8 da Silva2', 'Engenharia Mecanica', 
'UFSC', 'Silva3 da Pessoa7', 2008, 2011, 'Marshall Mathers' ] ).
formacaoNoCursoDe( ['Pessoa1 da Silva1', 'Ciencias da Computacao', 
'UFSC', 'Silva9 da Pessoa1', 2007, 2012, 'Bruno Mars', 'Rock Star' ] ).
formacaoNoCursoDe( ['Pessoa2 da Silva9', 'Ciencias da Automacao', 
'UFMG', 'Silva8 da Pessoa2', 2005, 2010 ] ).
formacaoNoCursoDe( ['Pessoa3 da Silva7', 'Engenharia da Computacao', 
'USP', 'Silva7 da Pessoa3', 2009, 2015, 'Silvio Santos' ] ).
formacaoNoCursoDe( ['Pessoa4 da Silva7', 'Ciencias Biologicas', 
'UNICAMP', 'Silva6 da Pessoa4', 2002, 2012, 'Silvio Santos', 'Bruno Mars' ] ).
formacaoNoCursoDe( ['Pessoa5 da Silva7', 'Engenharia da Computacao', 
'UFSC', 'Silva5 da Pessoa5', 2000, 2005, 'Roberto Carlos' ] ).
formacaoNoCursoDe( ['Pessoa6 da Silva5', 'Ciencias Juridicas', 
'UFSC', 'Silva4 da Pessoa5', 2002, 2012 ] ).
formacaoNoCursoDe( ['Pessoa6 da Silva4', 'Ciencias Sociais', 
'ITA', 'Silva4 da Pessoa6', 2007, 2013 ], 'Demi Lovato' ). 

/* Nome, data de nascimento, cidade, telefone 
 * */
informacoesPessoais( ['Pessoa1 da Silva1', date( 1990, 1, 10 ),  
'Sao Paulo', 4837326424 ] ).
informacoesPessoais( ['Pessoa2 da Silva9', date( 1977, 2, 15 ),  
'Florianopolis', 4737326422 ] ).
informacoesPessoais( ['Pessoa3 da Silva7', date( 1960, 5, 13 ), 
'Sao Paulo', 4737326427 ] ).
informacoesPessoais( ['Pessoa4 da Silva7', date( 1930, 12, 09 ), 
'Rio de Janeiro', 4887326455 ] ).
informacoesPessoais( ['Pessoa5 da Silva7', date( 1950, 10, 30 ), 
'Rio Branco', 4887326465 ] ).
informacoesPessoais( ['Pessoa6 da Silva5', date( 1990, 1, 10 ), 
'Florianopolis', 1177326475 ] ).
informacoesPessoais( ['Pessoa6 da Silva4', date( 1981, 8, 20 ), 
'Cuiaba', 1177326825 ] ).
informacoesPessoais( ['Pessoa8 da Silva2', date( 1982, 7, 04 ), 
'Joao Pessoa', 8197326625 ] ).
informacoesPessoais( ['Pessoa9 da Silva2', date( 1986, 6, 12 ), 
'Brasilia', 8177326825 ] ).
informacoesPessoais( ['Pessoa9 da Silva1', date( 1955, 11, 20 ), 
'Florianopolis', 4887347425 ] ).


/* Questão 1
 * Qual o telefone de uma dada pessoa?
 * Primeiro testo se a pessoa é a cabeça da lista.
 * Segundo pego o telefone dela nesta lista na quarta posição.
 * */
qualTelefone(Nome,Telefone) :- informacoesPessoais([Nome, _, _, Telefone]).

/* Questão 2
 * Quais as pessoas de uma dada cidade?
 * */
quemMora(Cidade, Nome) :- informacoesPessoais([Nome, _, Cidade, _]).

/* Questão 3
 * Qual a idade de uma dada pessoa?
 * Primeiro, converto a data de aniversário para um TimeStamp. 
 * Segundo, consigo a data atual e armazeno em outro TimeStamp2.
 * Terceiro, calculo a diferença de TimeStamps.
 * Quarto, converto o TimeStamp em idade.
 * */
privado_CalcularIdade(Data, Idade) :-
        date_time_stamp(Data, TimeStamp1),
        get_time(TimeStamp2),
        TempoDeVidaEmSegundos is TimeStamp2 - TimeStamp1,
        convert_time(TempoDeVidaEmSegundos,TimeStamp3,_,_,_,_,_,_),
        Idade is TimeStamp3 - 1970.

/* Pega a data de nascimento de uma dada pessoa e calcula sua idade 
 * Primeiro, encontra a pessoa na lista informacoesPessoais.
 * Segundo, pega sua DataDeNascimento da lista L de informações da Pessoa.
 * Terceiro, calcula sua idade.
 * */
calcularIdadeDe(Pessoa, Idade) :- 
	informacoesPessoais(L), privado_DadoNaPosicao(DadoDeRetorno,[_|L],1),
	DadoDeRetorno = Pessoa,
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade).
 
/* Questão 5
* Usando dividirLista para "caminhar" na lista até a posição do curso.
* A sublista criada excluirá tudo que vem antes, fazendo com que a cabeça
* da lista seja o curso, então imprimimos a cabeça.
*/
dividirLista(L,0,[],L).
dividirLista([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N-1, dividirLista(Xs,N1,Ys,Zs).

qualCurso(Nome,Curso) :-
formacaoNoCursoDe(List),
dividirLista(List, 1, _, [Curso|_]).

/* Questão 7
 * Quais os colegas de curso ou de trabalho de uma dada pessoa?
 * Primeiro eu tiro a parte inicial da lista.
 * Segundo imprimo o restante da lista, isto é, o nome das referências.
 * */
privado_DividirLista(L,0,[],L).
privado_DividirLista([X|Xs],N,[X|Ys],Zs) :- 
	N > 0, 
	N1 is N - 1, 
	privado_dividirLista(Xs,N1,Ys,Zs).

/* Verifica se uma pessoa é membro da cabeça da lista. Faz isso para garantir 
 * que não seja retornado verdadeiro caso uma das pessoas no final da lista
 * seja encontrada. 
 * */
privado_is_head_member(P, L) :- L = [P|_].

/* Informa quem tão os colegas de uma dada pessoa. Primeiro carrega a lista
 * de formacaoNoCursoDe em Lista, depois devifica se ela pertence a lista, 
 * e caso sim, retira retorna os colegas na variável Colegas. 
 * */
colegasDe(Pessoa, Colegas) :- 
	formacaoNoCursoDe(Lista), 
	privado_is_head_member(Pessoa, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas).













/* ######################### Biblioteca de funções ##########################*/
/* Verifica o comprimento de uma lista e imprime os passos intermediários
 * da contagem na tela em uma nova linha cada. */
my_length([],0).
my_length([_|L],N) :- my_length(L,N1), N is N1 + 1, write(N) ,writef('\n').

/* Dada uma posição K, dadoDeinforNaPosicao(Posicao, dadoDeRetorno), 
 * retorna um elemento em uma dada posição na lista definida no predicado 
 * informacoesPessoais([...]) */
privado_DadoNaPosicao(X,[X|_],0).
privado_DadoNaPosicao(X,[_|L],K) :- K > 0, K1 is K - 1, privado_DadoNaPosicao(X,L,K1).
dadoDeinforNaPosicao(Posicao, DadoDeRetorno) :- 
	informacoesPessoais(L), privado_DadoNaPosicao(DadoDeRetorno,[_|L],Posicao).


