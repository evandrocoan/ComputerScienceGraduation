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


/* ############################ Programa ###################################*/
/* Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 * Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
informacoesAcademicas( ['Pessoa9 da Silva1', 'Ciencias da Computacao', 
'ITA', 'Silva1 da Pessoa9', 2010, 2015 ] ).
informacoesAcademicas( ['Pessoa9 da Silva2', 'Ciencias da Computacao', 
'ITA', 'Silva2 da Pessoa8', 2003, 2007, 'Avril Lavigne', 'Demi Lovato' ] ).
informacoesAcademicas( ['Pessoa9 da Silva2', 'Sexologia', 
'ITA', 'Silva11 da Pessoa13', 2007, 2012, 'Madonna' ] ).
informacoesAcademicas( ['Pessoa8 da Silva2', 'Engenharia Mecanica', 
'UFSC', 'Silva3 da Pessoa7', 2008, 2011, 'Marshall Mathers' ] ).
informacoesAcademicas( ['Pessoa1 da Silva1', 'Ciencias da Computacao', 
'UFSC', 'Silva9 da Pessoa1', 2007, 2012, 'Bruno Mars', 'Rock Star' ] ).
informacoesAcademicas( ['Pessoa2 da Silva9', 'Ciencias da Automacao', 
'UFMG', 'Silva8 da Pessoa2', 2005, 2010 ] ).
informacoesAcademicas( ['Pessoa3 da Silva7', 'Engenharia da Computacao', 
'USP', 'Silva7 da Pessoa3', 2009, 2015, 'Silvio Santos' ] ).
informacoesAcademicas( ['Pessoa4 da Silva7', 'Ciencias Biologicas', 
'UNICAMP', 'Silva6 da Pessoa4', 2002, 2012, 'Silvio Santos', 'Bruno Mars' ] ).
informacoesAcademicas( ['Pessoa5 da Silva7', 'Engenharia da Computacao', 
'UFSC', 'Silva5 da Pessoa5', 2000, 2005, 'Roberto Carlos' ] ).
informacoesAcademicas( ['Pessoa6 da Silva5', 'Ciencias Juridicas', 
'UFSC', 'Silva4 da Pessoa5', 2002, 2012 ] ).
informacoesAcademicas( ['Pessoa6 da Silva4', 'Ciencias Sociais', 
'ITA', 'Silva11 da Pessoa6', 2007, 2013 , 'Demi Lovato' ] ). 
informacoesAcademicas( ['Pessoa6 da Silva4', 'Ciencias Sociais', 
'ITA', 'Silva19 da Pessoa24', 2007, 2013 , 'Maroon 5', 'Clean Bandit' ] ). 
informacoesAcademicas( ['Pessoa6 da Silva4', 'Ciencias Sociais', 
'ITA', 'Silva15 da Pessoa16', 2007, 2013 , 'Avenged Sevenfold', 'Eminem' ] ). 

/* Nome, data de nascimento, cidade, telefone 
 * */
informacoesPessoais( ['Pessoa1 da Silva1', date( 1990, 1, 10 ),  
'Sao Paulo', 4837326424 ] ).
informacoesPessoais( ['Pessoa2 da Silva9', date( 1977, 2, 15 ),  
'Florianopolis', 4737326422 ] ).
informacoesPessoais( ['Pessoa3 da Silva7', date( 1971, 5, 13 ), 
'Sao Paulo', 4737326427 ] ).
informacoesPessoais( ['Pessoa4 da Silva7', date( 1978, 12, 09 ), 
'Rio de Janeiro', 4887326455 ] ).
informacoesPessoais( ['Pessoa5 da Silva7', date( 1982, 10, 30 ), 
'Rio Branco', 4887326465 ] ).
informacoesPessoais( ['Pessoa6 da Silva5', date( 1990, 1, 10 ), 
'Florianopolis', 1177326475 ] ).
informacoesPessoais( ['Pessoa6 da Silva4', date( 1981, 8, 20 ), 
'Cuiaba', 1177326825 ] ).
informacoesPessoais( ['Pessoa8 da Silva2', date( 1982, 7, 04 ), 
'Joao Pessoa', 8197326625 ] ).
informacoesPessoais( ['Pessoa9 da Silva2', date( 1986, 6, 12 ), 
'Brasilia', 8177326825 ] ).
informacoesPessoais( ['Pessoa9 da Silva1', date( 1976, 11, 20 ), 
'Florianopolis', 4887347425 ] ).


/* Questão 1 ###############################################################
 * Qual o telefone de uma dada pessoa?
 * 
 * Primeiro testo se a pessoa é a cabeça da lista.
 * Segundo pego o telefone dela nesta lista na quarta posição.
 * */
qualTelefoneDe(Nome,Telefone) :- informacoesPessoais([Nome, _, _, Telefone]).

/* Questão 2 ###############################################################
 * Quais as pessoas de uma dada cidade?
 * */
quemMoraEm(Cidade, Nome) :- informacoesPessoais([Nome, _, Cidade, _]).

/* Questão 3 ###############################################################
 * Qual a idade de uma dada pessoa?
 * 
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
qualIdadeDe(Nome, Idade) :- 
	informacoesPessoais(L), privado_DadoNaPosicao(NomeDaPessoa,[_|L],1),
	NomeDaPessoa = Nome,
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade).
	
/* Questão 4 ###############################################################
 * Quais as pessoas com mais de 30 anos?
 * 
 * Primeiro, carrego a Lista com os dados da pessoa.
 * Segundo, calculo a idade da pessoa.
 * Terceiro, calculo se a pessoa é maior que 30 anos.
 * Quarto, pego o nome da pessoa e retorno na variável Nome.
 * */
quaisComMaisDe30Anos(Nome) :-
	informacoesPessoais(L),
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade),
	Idade > 30,
	privado_DadoNaPosicao(Nome,[_|L],1).

/* Questão 5 ###############################################################
 * Quais os cursos de uma dada pessoa?
 * 
 * Primeiro encontro a pessoa na lista.
 * Segundo pego o curso dela e retorna na variável Curso.
 * */
qualCursoDe(Nome, Curso) :-
	informacoesAcademicas(L), privado_DadoNaPosicao(DadoDeRetorno,[_|L],1),
	DadoDeRetorno = Nome,
	privado_DadoNaPosicao(Curso,[_|L],2).

/* Questão 6 ###############################################################
 * Quais os(as) orientadores(as) de uma dada pessoa?
 * 
 * Primeiro, pega a lista.
 * Segundo, pega o nome da pessoa e verifica se é a pessoa que queremos.
 * Terceiro, pega o nome do orientador.
 * */
quaisOrientadoresDe(Nome, Orientadores) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(NomeDaPessoa, [_|L], 1),
	NomeDaPessoa = Nome,
	privado_DadoNaPosicao(Orientadores, [_|L], 4).

/* Questão 7 ###############################################################
 * Quais os colegas de curso ou de trabalho de uma dada pessoa?
 * 
 * Primeiro, tiro a parte inicial da lista.
 * Segundo, imprimo o restante da lista, isto é, o nome das referências.
 * */
privado_DividirLista(L,0,[],L).
privado_DividirLista([X|Xs],N,[X|Ys],Zs) :- 
	N > 0, 
	N1 is N - 1, 
	privado_DividirLista(Xs,N1,Ys,Zs).

/* Verifica se uma pessoa é membro da cabeça da lista. Faz isso para garantir 
 * que não seja retornado verdadeiro caso uma das pessoas no final da lista
 * seja encontrada. 
 * */
privado_is_head_member(P, L) :- L = [P|_].

/* Informa quem são os colegas de uma dada pessoa. Primeiro carrega a lista
 * de informacoesAcademicas em Lista, depois verifica se ela pertence à lista. 
 * Caso sim, retorna os colegas na variável Colegas. 
 * */
quaisColegasDe(Nome, Colegas) :-
	informacoesAcademicas(Lista), 
	privado_is_head_member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas).

/* Questão 8 ###############################################################
 * Quais as pessoas sem nenhum colega citado como referencia?
 * 
 * Primeiro, carrega-se a lista em L, pega-se uma pessoa e tira-se a parte 
 * inicial da lista.
 * Segundo, verifica-se o restante da lista, isto é, o nome das referências, 
 * é uma lista vazia, ou seja, caso tenha comprimento 0.
 * */
quaisNaoTemRerefencia(Nomes) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(Nomes, [_|L], 1),
	quaisColegasDe(Nomes, Colegas),
	length(Colegas, Tamanho),
	Tamanho = 0.

/* Questão 9
 * Qual o numero de colegas de uma dada pessoa?
 * 
 * Primeiro, carrega-se a informacoesAcademicas em Lista, 
 * Segundo, verifica-se se essa é a lista dessa pessoa.
 * Terceiro, retira-se da lista as outras informações, deixando somente os 
 * colegas.
 * Quarto, coloca-se o comprimento da lista, isto é, o número de colegas de
 * uma dada pessoa em um dado curso.
 * */
qualNumeroDeColegas(Nome, Quantidade) :-
	informacoesAcademicas(Lista), 
	privado_is_head_member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas),
	length(Colegas, Quantidade).









