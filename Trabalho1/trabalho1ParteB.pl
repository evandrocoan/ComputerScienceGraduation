importarTrabalho1ParteA:-[trabalho1ParteA].
:-importarTrabalho1ParteA.

/* Questão 16 ###############################################################
 * Quais as pessoas que estudaram em uma dada instituição?
 *
 * Eu passo em cada pessoa, e vejo se a instituição dela é a que queremos.
 * Caso sim, eu pego a pessoa retorno, caso não, retorno a string vazia.
 * */
quemEstudouNaInstituicao(Instituicao, Nomes) :-
    /* Faz todas as requisições ';' para a variavel informacoesAcademicas e
     * recebe um lista Lista contendo o nome das pessoas.
     * */
    findall(Nome, privado_QuemEstudouNaInstituicao(Instituicao, Nome), Nomes).

	/* Este predicado retorna sempre o nome da pessoa, caso ela tenha estudado em 
	 * uma dada instituição armazenado na variável informacoesAcademicas.
	 * */
	privado_QuemEstudouNaInstituicao(Instituicao_Interno, Nome_Interno) :-
	    informacoesAcademicas(L), 
	    dadoNaPosicao(InstituicaoDaPessoa, L, 2),
	    InstituicaoDaPessoa == Instituicao_Interno, 
	    dadoNaPosicao(Nome_Interno, L, 0).


/* Questão 17 ###############################################################
 * Qual o curso mais longo de uma dada pessoa?
 * 
 * Pega uma lista com os tempos do curso de uma dada pessoa.
 * Descobre qual a posição do curso com maior duração.
 * Cria uma lista com os cursos dessa dada pessoa.
 * Pega a retorna o curso que se encontra na posição de maior.
 * */
qualCursoMaisLongoDe( Nome, CursoLista ) :-
	/* Faz todas as requisições, e cria uma lista com os cursos de maior 
	 * duração, isto é, os cursos com o mesmo tempo de duração.
	 * */
	findall(Curso, privado_QualCursoMaisLongoDe( Nome, Curso ), CursoLista).
	
	privado_QualCursoMaisLongoDe( Nome, Curso ) :- 
		/* Pega os tempos dos cursos e coloca na ListaTempos.
		 * */
	    findall(Curso, tempoDeEstudoEmCadaCurso(Nome, Curso), ListaTempos),
	    /* Pega a posição do curso de maior duração da pessoa.
	     * */
	    posicaoDoMaior(ListaTempos, Posicao),
	   	/* Pega uma lista com dos cursos e coloca na ListaCursos.
		 * */
	    quaisCursosDe(Nome, ListaCursos), 
	    /* Pega qual o curso de maior duração da ListaDeCursos.
	     * */
	    dadoNaPosicao(Curso, ListaCursos, Posicao). 


/* Questão 18 ###########################################################
 * Qual a instituição de ensino com maior número de pessoas?
 * 
 * Primeiro pego a instituição de cada pessoa do banco de dados e crio
 * uma nova lista.
 * Segundo, aplico o predicado listMax que retorna o elemento da lista
 * com o maior número de ocorrências.
 * */
qualInstituicaoEnsinoComMaisPessoas(Instituicao) :-
	findall(Instit, privado_criarListaDeInstituicoes(Instit), Inst),
	listMax(Inst,Instituicao).
	
	privado_criarListaDeInstituicoes(Instituicao) :-
	informacoesAcademicas(L),
	dadoNaPosicao(Instituicao, [_|L], 3).


/* Questão 21 ###########################################################
 * Qual a pessoa mais citada como referência? Exiba seu currículo.
 * O algoritmo cria, inicialmente, duas listas de listas, uma com todas
 * as referências acadêmicas e outra com todas as referências
 * profissionais.
 * Após, as listas são concatenadas em uma única lista de listas, que é,
 * então, transformada em uma lista simples usando o predicado append.
 * listmax é utilizado no final para nos retornar o elemento com maior
 * número de ocorrência nesta lista.
 * */
 qualReferenciaMaisCitada(Referencia) :-
 findall(Ref, privado_PegarReferenciasAcademicas(Ref), Lista),
 findall(Ref2, privado_PegarReferenciasProfissionais(Ref2), Lista2),
 concatenadas(Lista, Lista2, ListaRef),
 append(ListaRef, ListaRefCompleta),
 listmax(ListaRefCompleta,Referencia).
 
/* Este predicado divide a lista de 'informacoesAcademicas' em duas
 * listas, onde uma delas é uma lista de Referencias. 
 * */
 privado_PegarReferenciasAcademicas(Ref) :-
 informacoesAcademicas(L),
 dividirLista(L,6,L1,Ref).
 
/* Este predicado divide a lista de 'informacoesProfissionais' em duas
 * listas, onde uma delas é uma lista de Referencias. 
 * */
 privado_PegarReferenciasProfissionais(Ref) :-
 informacoesProfissionais(L),
 dividirLista(L,5,L1,Ref).
 
