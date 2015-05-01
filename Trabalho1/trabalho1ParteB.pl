importarTrabalho1ParteA:-[trabalho1ParteA].
:-importarTrabalho1ParteA.

/* ############################ Programa ###################################*/
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
 * Pega uma lista com os tempo do curso de uma dada pessoa.
 * Descobre qual a posição do curso com maior duração.
 * Cria uma lista com os cursos dessa dada pessoa.
 * Pega a retorna o curso que se encontra na posição de maior.
 * */
qualCursoMaisLongoDe( Nome, Curso ) :-
	/* Pega os tempos dos cursos e coloca na ListaTempos.
	 * */
    findall(Curso, privado_QualCursoMaisLongoDe(Nome, Curso), ListaTempos),
    /* Pega 
     * */
    findall(Curso, privado_QualCursoMaisLongoDe2(Nome, Curso), ListaCursos),
    maiorElemento(ListaTempos, Maior). 
    

/* Primeiro encontro a pessoa na lista.
 * Segundo pego o tempo de curso dela e retorna na variável Curso.
 * */ 
privado_QualCursoMaisLongoDe(Nome_Interno, Tempo) :-
    informacoesAcademicas(L), 
    dadoNaPosicao(Nome,L,0),
    Nome == Nome_Interno, 
    dadoNaPosicao(TempoInicial, L, 4),
    dadoNaPosicao(TempoFinal, L, 5),
    Tempo is TempoFinal - TempoInicial.

 
/* Questão 18 ###########################################################
 * Qual a instituição de ensino com maior número de pessoas?
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
	
