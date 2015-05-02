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
     *   recebe um lista Lista contendo o nome das pessoas.
     * */
    findall(Nome, privado_QuemEstudouNaInstituicao(Instituicao, Nome), Nomes).

	/* Este predicado retorna sempre o nome da pessoa, caso ela tenha estudado
	 *   em uma dada instituição armazenado na variável informacoesAcademicas.
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
	 *   duração, isto é, os cursos com o mesmo tempo de duração.
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
 *   uma nova lista.
 * Segundo, aplico o predicado listMax que retorna o elemento da lista
 *   com o maior número de ocorrências.
 * */
qualInstituicaoEnsinoComMaisPessoas(Instituicao) :-
	findall(Instit, privado_criarListaDeInstituicoes(Instit), Inst),
	listMax(Inst,Instituicao).
	
	privado_criarListaDeInstituicoes(Instituicao) :-
	informacoesAcademicas(L),
	dadoNaPosicao(Instituicao, [_|L], 3).


/* Questão 19 ###########################################################
 * Qual a empresa com maior tempo total de serviço? Considere a soma do 
 *   tempo de serviço de cada pessoa do banco de dados que trabalhou nesta 
 *   empresa. 
 *
 * Primeiro, para cada pessoa no lista você pega as empresas que ela trabalhou
 *   e coloca em uma lista NomesDasEmpresas, que não aceita repetição.
 * Segundo, para cada empresa na lista NomesDasEmpresas, você coloca na lista
 *   TemposDasEmpresas e na mesma posição da empresa NomesDasEmpresas, o 
 *   tempo total de trabalho das pessoas naquela empresa.
 * Para isso você passa em todas as pessoas do banco de dados e caso essa 
 *   pessoa tenha trabalho na empresa, você coloca o tempo de trabalho dessa 
 *   pessoa na lista TemposDasEmpresas e na posição correspondente a empresa 
 *   em questão. 
 * Isto é, estas listas contém os dados na mesmo posição, por exemplo, o tempo
 *   de trabalho da empresa 1 está na posição 1 da lista TemposDasEmpresas e o 
 *   nome da empresa 1 está na posição 1 da lista NomesDasEmpresas.
 * */
maiorTempoDeServico(Empresa) :-
	/* Passo 1, colocar o nome de todas as empresas em uma lista ordenada
	 *   NomesDasEmpresas e sem repetição. 
	 * */
	privado_CarregaLista(NomesDasEmpresas),
	/* Passo 2, chamo um predicado que recursivamente, pega a cabeça da lista 
	 *   NomesDasEmpresas e chama o predicado privado_TempoTotalDeTrabalho e 
	 *   salva o TempoTotalDeTrabalho da empresa cabeça da lista.
	 *   NomesDasEmpresas na lista variável TemposDasEmpresas.
	 * */
	privado_CarregarTempoDeTrabalho(NomesDasEmpresas, TemposDasEmpresas),
	/* Descobre qual a posição da empresa com maior tempo .
	 * */
	posicaoDoMaior(TemposDasEmpresas, Posicao),
    /* Pega o nome da empresa com maior tempo de trabalho. 
     * */
    dadoNaPosicao(Empresa, NomesDasEmpresas, Posicao),
    /* Encerra a execução do algoritmo.
     * */
    !.
	
	/* Pega a lista NomesDasEmpresas_Interno, e chama o predicado 
	 *   privado_CarregarTempoDeTrabalho.
	 * */
	privado_CarregarTempoDeTrabalho(ListaNomes, ListaTempos) :-
		/* Chama o predicado que realizada a recursão.
		 * */
		privado_CarregarTempoDeTrabalho(ListaNomes, [], ListaTempos).
	
	privado_CarregarTempoDeTrabalho([], Cauda, ListaTempos) :-
		/* Salva a lista TemposDasEmpresas.
		 * */
		copy_term(Cauda, ListaTempos),
		!.
	
	privado_CarregarTempoDeTrabalho(Lista, ListaTempoEntrada, ListaTempos) :-
		/* Divide a Lista entre Cabeca e Cauda.
		 * */
		dividirLista(Lista, 1, CabecaLista, Cauda), 
		/* Transforma a lista de um elemento CabecaLista em um elemento 
		 *   Cabeca. 
		 * */
		dadoNaPosicao(Cabeca, CabecaLista, 0), 
		/* Chama o predicado privado_TempoTotalDeTrabalho. 
		 * */
		privado_TempoTotalDeTrabalho(Cabeca, Tempo), 
		/* Adiciona na ListaTempo o tempo da empresa em Cabeca. 
		 * */
		inseridoNoFinal(Tempo, ListaTempoEntrada, ListaTempoSaida), 
		/* Chama recursivamente este predicado para processar o resto da 
		 *   lista. 
		 * */
		privado_CarregarTempoDeTrabalho(Cauda, ListaTempoSaida, ListaTempos).
	
	/* Dado o NomeDaEmpresa, calculo o tempo total.
	 * */
	privado_TempoTotalDeTrabalho( NomeDaEmpresa, TempoTotal ) :-
		/* Faz todas as requisições ';' para o predicado 
		 *   privado_QualTempoDeTrabalho e cria uma lista de TemposTrabalho.
		 * */
		findall(Tempo, privado_QualTempoDeTrabalho(NomeDaEmpresa, Tempo), 
		        TemposTrabalho),
		/* Faz a soma dos TemposTrabalho e retorna na variável TempoTotal.
		 * */
		somaDosElementos(TemposTrabalho, TempoTotal).
		
		/* Retorna o tempo de trabalho em uma empresa NomeDaEmpresa
		 *   armazenada no predicado informacoesProfissionais. 
		 * A cada vez que se faz um requisição ';' a este predicado, ele 
		 *   retorna a o tempo da empresa NomeDaEmpresa, caso a empresa 
		 *   pega no predicado informacoesProfissionais não seja NomeDaEmpresa, 
		 *   retorna 0 em TempoDeTrabalho, caso contrário, retorna o 
		 *   TempoDeTrabalho.
		 * */
		privado_QualTempoDeTrabalho(NomeDaEmpresa, TempoDeTrabalho) :-
			/* Primeiro, carrego a lista de informacoesProfissionais. 
			 * */
			informacoesProfissionais(L),
			/* Segundo, pego o nome da empresa. 
			 * */
			dadoNaPosicao(Nome, L, 1),
			/* Terceiro, verifico se o Nome carregado é o NomeDaEmpresa que 
			 * queremos. 
			 * */
			( NomeDaEmpresa = Nome ->
				/* Quarto, carregamos o tempo de trabalho da pessoa na variável 
				 * TempoDeTrabalho. 
				 * */
				dadoNaPosicao(TempoInicial, L, 3),
				dadoNaPosicao(TempoFinal,L,4),
				TempoDeTrabalho is TempoFinal - TempoInicial
			;
				/* Ajusto o valor padrão de TempoDeTrabalho 
				* */
				TempoDeTrabalho is 0
			).

	/* Cria um lista contendo todas as empresas.
	 * */
	privado_CarregaLista( ListaDeEmpresas ) :-
		/* Faz todas as requisições ';' para o predicado privado_QualEmpresa.
		 * */
		findall( Empresa, privado_QualEmpresa( Empresa ), Lista ),
		/* Retira desta lista todos os repetidos e retorna essa lista em 
		 *   ListaDeEmpresas.
		 * */
		sort(Lista, ListaDeEmpresas).
	
		/* Retorna o Nome da Empresa armazenada no predicado 
		 *   informacoesProfissionais. A cada vez que se faz um requisição ';' 
		 *   a este predicado, ele retorna a próxima empresa, até não existirem 
		 *   mais informacoesProfissionais.
		 * */
		privado_QualEmpresa(NomeDaEmpresa) :-
			/* Primeiro carrego a lista de informacoesProfissionais. 
			 * */
			informacoesProfissionais(L),
			/* Segundo pego o nome da empresa. 
			 * */
			dadoNaPosicao(NomeDaEmpresa, L, 1).


/* Questão 21 ###########################################################
 * Qual a pessoa mais citada como referência? Exiba seu currículo.
 * 
 * O algoritmo cria, inicialmente, duas listas de listas, uma com todas
 *   as referências acadêmicas e outra com todas as referências
 *   profissionais.
 * Após, cada lista é transformada em uma lista simples usando append,
 *   e então são concatenadas em apenas uma única lista.
 * Por fim, esta lista é passada pelo predicado listmax que nos retorna
 *   o elemento de maior ocorrência.
 * */
qualReferenciaMaisCitada(Referencia) :-
	findall(Ref, privado_PegarReferenciasAcademicas(Ref), Lista),
	findall(Ref2, privado_PegarReferenciasProfissionais(Ref2), Lista2),
	append(Lista, ListaRef),
	append(Lista2, ListaRef2),
	concatenadas(ListaRef, ListaRef2, ListaRefCompleta),
	listMax(ListaRefCompleta,Referencia).

	/* Este predicado divide a lista de 'informacoesAcademicas' em duas
	 * listas, onde uma delas é uma lista de Referencias. 
	 * */
	privado_PegarReferenciasAcademicas(Ref) :-
		informacoesAcademicas(L),
		dividirLista(L,6,_,Ref).

	/* Este predicado divide a lista de 'informacoesProfissionais' em duas
	 * listas, onde uma delas é uma lista de Referencias. 
	 * */
	privado_PegarReferenciasProfissionais(Ref) :-
		informacoesProfissionais(L),
		dividirLista(L,5,_,Ref).


/* Questão 22 ##########################################################
  * Qual a pessoa com maior quantidade de colegas referenciados (de curso
  * ou de trabalho)? Exiba seu currículo.
  * Este predicado não recebe entradas.
  * Ele criará uma lista com todas as pessoas do banco de dados e a
  * seguir criará outra lista de valores numéricos que correspondem,
  * baseado na sua posição, à quantidade de referências da pessoa que se
  * encontra na lista de pessoas, na mesma posição que o valor numérico.
  * Após, é encontrado o maior valor na lista numérica, encontrado o seu
  * índice e retornado o nome da pessoa de mesmo índice na lista de 
  * pessoas.
  *  */
 qualPessoaComMaiorQtdReferencias(Resposta, Quantidade) :-
	privado_MakeList(Pessoas),
	privado_CarregarReferencias(Pessoas),
	nb_getval(qtdReferencias, ListaRef),
	maiorElemento(ListaRef, Quantidade),
	indexOf(ListaRef, Quantidade, I),
	dadoNaPosicao(Resposta, Pessoas, I).
 
/* Predicado que cria uma lista com todas as pessoas do banco de dados.
 */
 privado_MakeList(Pessoas) :-
	findall(Nome, privado_Pessoas(Nome), Lista),
	sort(Lista, Pessoas).
		
/* Predicado auxiliar para privado_MakeList(Pessoas); este adquire o
 * nome da pessoa.
 */
 privado_Pessoas(Nomes) :-
	informacoesAcademicas(L),
	dadoNaPosicao(Nomes, L, 0).
	
/* Pega a lista Referencias_Interno, e chama o predicado 
 *   privado_CarregarReferencias.
 * */
 privado_CarregarReferencias(Referencias_Interno) :-
/* Chama o predicado que realizada a recursão.
 * */
 privado_CarregarReferencias(Referencias_Interno, []).
 
/* Salva a lista qtdReferencias.
  * */
 privado_CarregarReferencias([], Cauda) :-
	nb_setval(qtdReferencias, Cauda),
	!.
	
/* Predicado que criará a lista com a quantidade de referencias de cada
 * pessoa.
 * */
 privado_CarregarReferencias(Lista, Lista2) :-
/* Divido a lista em cabeça(contendo o nome da pessoa) e cauda(resto).
 * */
	dividirLista(Lista, 1, CabecaLista, Cauda),
/* Pego o nome da pessoa.
 * */
	dadoNaPosicao(Cabeca, CabecaLista, 0),
/* Calculo o seu total de referências profissionais.
 */
	privado_TotalReferenciasP(Cabeca, Aux1),
/* Calculo o seu total de referências academicas.
 * */
	privado_TotalReferenciasA(Cabeca, Aux2),
/* Calculo o seu total de referências.
 * */
	Ref is Aux1 + Aux2,
/* Insiro na lista de referências.
 * */
	inseridoNoFinal(Ref, Lista2, ListaRef),
/* Realizo a recursão.
 * */
	privado_CarregarReferencias(Cauda, ListaRef).
	
/* Predicado que calcula o total de referencias profissionais de uma dada
 * pessoa.
 * */	
privado_TotalReferenciasP(NomePessoa, TotalRef) :-
/* Faz todas as requisições ';' para o predicado 
 *   privado_QtdReferenciasP e cria uma lista de TotalRef.
 * */
	findall(Qtd, privado_QtdReferenciasP(NomePessoa, Qtd), List),
	somaDosElementos(List, TotalRef).
	
/* Retorna a quantidade de referências profissionais de uma pessoa do
 * banco de dados em informacoesProfissionais. 
 * A cada vez que se faz um requisição ';' a este predicado, ele 
 * retorna a quantidade de referências caso Pessoa(nome) seja igual
 * ao Nome encontrado em informacoesProfissionais, caso não seja, é 
 * retornado 0 em QtdRef
 * */
 privado_QtdReferenciasP(Pessoa, QtdRef) :-
/* Primeiro carrego uma lista de informacoesProfissionais.
 * */
	informacoesProfissionais(L),
/* Segundo, pego um Nome.
 * */
	dadoNaPosicao(Nome, L, 0),
/* Terceiro, confiro se Pessoa e Nome são iguais.
 * */
	(Pessoa = Nome ->
/* Caso sim, divido a lista para pegar apenas suas referências e calculo
 * o seu tamanho.
 * */
		dividirLista(L,5,L1,L2),
		length(L2,N),
		QtdRef is N
	;
/* Caso não, retorno 0.
 * */
		QtdRef is 0
	).
	
/* Predicado que calcula o total de referencias academicas de uma dada
 * pessoa.
 * */
privado_TotalReferenciasA(NomePessoa, TotalRef) :-
/* Faz todas as requisições ';' para o predicado 
 *   privado_QtdReferenciasA e cria uma lista de TotalRef.
 * */
	findall(Qtd, privado_QtdReferenciasA(NomePessoa, Qtd), List),
	somaDosElementos(List, TotalRef).
/* Retorna a quantidade de referências acadêmicas de uma pessoa do
 * banco de dados em informacoesAcademicas. 
 * A cada vez que se faz um requisição ';' a este predicado, ele 
 * retorna a quantidade de referências caso Pessoa(nome) seja igual
 * ao Nome encontrado em informacoesAcademicas, caso não seja, é 
 * retornado 0 em QtdRef
 * */	
 privado_QtdReferenciasA(Pessoa, QtdRef) :-
/* Primeiro carrego uma lista de informacoesAcademicas.
 * */
	informacoesAcademicas(L),
/* Segundo, pego um Nome.
 * */
	dadoNaPosicao(Nome, L, 0),
/* Terceiro, confiro se Pessoa e Nome são iguais.
 * */
	(Pessoa = Nome ->
/* Caso sim, divido a lista para pegar apenas suas referências e calculo
 * o seu tamanho.
 * */
		dividirLista(L,6,L1,L2),
		length(L2,N),
		QtdRef is N
	;
/* Caso não, retorno 0.
 * */
		QtdRef is 0
	).
