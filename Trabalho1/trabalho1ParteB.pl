importarTrabalho1ParteA:-[trabalho1ParteA].
:-importarTrabalho1ParteA.

/* ############################ Programa ###################################*/
/* Questão 1 ###############################################################
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




