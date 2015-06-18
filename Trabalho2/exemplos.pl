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

lerArquivo :- 
	open('Trabalho1/bancoDeDados.pl', read, X),
	
	% salva a entrada padrão, para restaurar mais tarde.
	current_input(Stream),
	set_input(X),
	
	write(X),
	
	set_input(Stream),
	close(X).

% open(’arq.saida’, write, X).


main :-
    open('Trabalho1/bancoDeDados.pl', read, Str),
    read(Str, X),
    write(X).

get_userinfo(personal(Name, Gender, Age, Attr)) :-
      write('Enter name: '),
      readln([Name|_]),
      write('Enter gender: '),
      readln([Gender|_]),
      write('Enter age: '),
      readln([Age|_]),
      write('Enter attr: '),
      readln([Attr|_]).

record_userinfo(Person) :-
      append('Trabalho1/bancoDeDados.pl'),
      write(Person), nl,
      told.

teste :- 
	consult('Trabalho1/bancoDeDados.pl'),
	assert(informacoesProfissionais('asdfkasjdfl','asldkfjsdlafkj')), 
	
	/* Lista todas as clausulas informacoesProfissionais e grava no arquivo */
	tell('Trabalho1/bancoDeDados.pl'),   
	listing(informacoesAcademicas),
	listing(informacoesPessoais), 
	listing(informacoesProfissionais),
	told.

incluirDadosPara(Nome) :-
    write('Entre com o nome do novo dado, iniciado com letra minuscula\n'), 
    write(', sem espacos e terminado por um : '), 
    read(NomeDoNovoDado), 
    write('Entre com o valor do novo dado DIGITE ENTRE ASPAS SIMPLES: '), 
    read(ValorDoNovoDado), 
    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [NomeDoNovoDado,Nome,ValorDoNovoDado],
    assert(Predicado), 
    
    /* carregarListaDePredicados( listaDePredicados ) */
    /* listaDePredicados <- add Predicado */
    /* ouvirPrecidados( listaDePredicados ) */
    
    /* Lista todas as clausulas informacoesProfissionais e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),   
    told.


/* Modelo de um 'for' que passa em todos os elementos da Lista e retorna o resultado do 
 *   processamento em Resultado.
 * */
privado__Recursao(Lista, Resultado) :-
    privado__RecursaoInterno(Lista, [], Resultado).

privado__RecursaoInterno([], Temporario, Resultado) :-
    copy_term(Temporario, Resultado).
    
privado__RecursaoInterno(Lista, AcumuladorEntrada, Resultado) :-
    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
    primeiro(ElementoTemporario, ElementoAtual),
    
    % Fazer as operações sobre o ElementoAtual da lista.
    

    privado__RecursaoInterno(RestoLista, AcumuladorSaida, Resultado).








