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


/* 19. Qual a empresa com maior tempo total de serviço? Considere a soma do 
 * tempo de serviço de cada pessoa do banco de dados que trabalhou nesta 
 * empresa. 
 *
 * Primeiro, para cada pessoa no lista você pega as empresas que ela trabalhou
 *  e coloca em uma lista NomesDasEmpresas, que não aceita repetição.
 * Segundo, para cada empresa na lista NomesDasEmpresas, você coloca na lista
 *  TemposDasEmpresas e na mesma posição da empresa NomesDasEmpresas, o 
 *  tempo total de trabalho das pessoas naquela empresa.
 * Para isso você passa em todas as pessoas do banco de dados e caso essa 
 *  pessoa tenha trabalho na empresa, você coloca o tempo de trabalho dessa 
 *  pessoa na lista TemposDasEmpresas e na posição correspondente a empresa 
 *  em questão. 
 * Isto é, estas listas contém os dados na mesmo posição, por exemplo, o tempo
 *  de trabalho da empresa 1 está na posição 1 da lista TemposDasEmpresas e o 
 *  nome da empresa 1 está na posição 1 da lista NomesDasEmpresas.
 * */
maiorTempoDeServico(Empresa) :-
	/* Passo 1, colocar o nome de todas as empresas em uma lista ordenada
	 *  NomesDasEmpresas e sem repetição. 
	 * */
	privado_CarregaLista(NomesDasEmpresas),
	/* Passo 2, chamo um predicado que recursivamente, pega a cabeça da lista 
	 *  NomesDasEmpresas e chama o predicado privado_TempoTotalDeTrabalho e 
	 *  salva o TempoTotalDeTrabalho da empresa cabeça da lista na lista 
	 *  variável global temposDasEmpresas.
	 * */
	privado_CarregarTempoDeTrabalho(NomesDasEmpresas),
	/* Pega a lista temposDasEmpresas 
	 * */
	nb_getval(temposDasEmpresas, TemposDasEmpresas),
	/* Descobre qual a posição da empresa com maior tempo 
	 * */
	posicaoDoMaior(TemposDasEmpresas, Posicao),
    /* Pega o nome da empresa com maior tempo de trabalho. 
     * */
    dadoNaPosicao(Empresa, NomesDasEmpresas, Posicao),
    /* Encerra a execução do algoritmo.
     * */
    !.
	
	/* Pega a cabeça da lista NomesDasEmpresas_Interno, e chama o predicado 
	 * privado_TempoTotalDeTrabalho, e salva o TempoTotalDeTrabalho em 
	 * TemposDasEmpresas_Interno.
	 * */
	privado_CarregarTempoDeTrabalho(Lista) :-
		privado_CarregarTempoDeTrabalho(Lista, []).
	privado_CarregarTempoDeTrabalho([], Cauda) :-
		nb_setval( temposDasEmpresas, Cauda),
		!.
	privado_CarregarTempoDeTrabalho(Lista, ListaTempoEntrada) :-
		/* Divide a Lista entre Cabeca e Cauda 
		 * */
		dividirLista(Lista, 1, CabecaLista, Cauda), 
		/* Transforma a lista de um elemento CabecaLista em um elemento 
		 *  Cabeca. 
		 * */
		dadoNaPosicao(Cabeca, CabecaLista, 0), 
		/* Chama o predicado privado_TempoTotalDeTrabalho. 
		 * */
		privado_TempoTotalDeTrabalho(Cabeca, Tempo), 
		/* Adiciona na ListaTempo o tempo da empresa em Cabeca. 
		 * */
		inseridoNoFinal(Tempo, ListaTempoEntrada, ListaTempoSaida), 
		/* Chama recursivamente este predicado para processar o resto da 
		 *  lista. 
		 * */
		privado_CarregarTempoDeTrabalho(Cauda, ListaTempoSaida).

	/* Dado o NomeDaEmpresa, faz todas as requisições ';' para o predicado 
	 *  privado_QualTempoDeTrabalho e cria uma lista de TemposTrabalho.
	 * Depois faz a soma dos TemposTrabalho e retorna na variável TempoTotal.
	 * */
	privado_TempoTotalDeTrabalho( NomeDaEmpresa, TempoTotal ) :-
		findall(Tempo, privado_QualTempoDeTrabalho(NomeDaEmpresa, Tempo), 
		        TemposTrabalho),
		somaDosElementos(TemposTrabalho, TempoTotal).
		
		/* Retorna o tempo de trabalho em uma empresa NomeDaEmpresa
		 *  armazenada no predicado informacoesProfissionais. 
		 * A cada vez que se faz um requisição ';' a este predicado, ele 
		 *  retorna a o tempo da empresa NomeDaEmpresa, caso a empresa 
		 *  pega no predicado informacoesProfissionais não seja NomeDaEmpresa, 
		 *  retorna 0 em TempoDeTrabalho, caso contrário, retorna o 
		 *  TempoDeTrabalho.
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

	/* Faz todas as requisições ';' para o predicado privado_QualEmpresa e 
	 *  cria um lista contendo todas as empresas.
	 * Depois retira desta lista todos os repetidos e retorna essa lista em 
	 *  ListaDeEmpresas.
	 * */
	privado_CarregaLista( ListaDeEmpresas ) :-
		findall( Empresa, privado_QualEmpresa( Empresa ), Lista ),
		sort(Lista, ListaDeEmpresas).
	
		/* Retorna o Nome da Empresa armazenada no predicado 
		 *  informacoesProfissionais. A cada vez que se faz um requisição ';' 
		 *  a este predicado, ele retorna a próxima empresa, até não existirem 
		 *  mais informacoesProfissionais.
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
 * O algoritmo cria, inicialmente, duas listas de listas, uma com todas
 * as referências acadêmicas e outra com todas as referências
 * profissionais.
 * Após, cada lista é transformada em uma lista simples usando append,
 * e então são concatenadas em apenas uma única lista.
 * Por fim, esta lista é passada pelo predicado listmax que nos retorna
 * o elemento de maior ocorrência.
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


