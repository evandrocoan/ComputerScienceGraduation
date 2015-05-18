/* Para funcionar, dever selecionar o working directory para uma pasta acima
 *   da pasta Trabalho2 executando o arquivo de configuração.
 *
 * Muda para o diretório de trabalho padrão.
 *
 * irParaDiretorioPadrao :- 
 *	
 *	working_directory(_, 'D:/Evandro/Archives/Dropbox/Aplicativos/
 *                     SoftwareVersioning/2015-1_ParadigmasDeProgramacao').   
 *
 * :-irParaDiretorioPadrao.
 * */
importarTrabalho1ParteB :- ['Trabalho1/trabalho1ParteB.pl'].
:-importarTrabalho1ParteB.

/* Incluir novo dado para uma pessoa. Ao chamar este predicado se passa o 
 *   nome da pessoa que se deseja incluir um novo dado. 
 * Após a chamada, o predicado pergunta qual o nome do novo dado. E em seguida
 *   se pergunta qual o dado que se quer adicionar.
 * */
incluirDadosPara(Nome) :-
	write('Entre com o nome do novo dado: '),
	readln(NomeDoNovoDado),
	write('Entre com o valor do novo dado: '),
	readln(ValorDoNovoDado),
    consult('Trabalho1/bancoDeDados.pl'),
	assert(NomeDoNovoDado(Nome,ValorDoNovoDado)), 
	
	/* Lista todas as clausulas informacoesProfissionais e grava no arquivo */
	tell('Trabalho1/bancoDeDados.pl'),   
	listing(informacoesAcademicas),
	listing(informacoesPessoais), 
	listing(informacoesProfissionais),
	told.







