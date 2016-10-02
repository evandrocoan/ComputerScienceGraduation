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
     *   recebe um lista Lista contendo o nome das pessoas. */
    findall(Nome, privado_QuemEstudouNaInstituicao(Instituicao, Nome), Nomes).

	/* Este predicado retorna sempre o nome da pessoa, caso ela tenha estudado
	 *   em uma dada instituição armazenado na variável 
	 *   informacoesAcademicas.
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
qualCursoMaisLongoDe(Nome, CursoLista) :-
	
	/* Faz todas as requisições, e cria uma lista com os cursos de maior 
	 *   duração, isto é, os cursos com o mesmo tempo de duração. */
	findall(Curso, privado_QualCursoMaisLongoDe(Nome, Curso), CursoLista).
	
	privado_QualCursoMaisLongoDe(Nome, Curso) :- 
		
		/* Pega os tempos dos cursos e coloca na ListaTempos. */
	    findall(Curso, tempoDeEstudoEmCadaCurso(Nome, Curso), ListaTempos),
	    
	    /* Pega a posição do curso de maior duração da pessoa. */
	    posicaoDoMaior(ListaTempos, Posicao),
	    
	   	/* Pega uma lista com dos cursos e coloca na ListaCursos. */
	    quaisCursosDe(Nome, ListaCursos), 
	    
	    /* Pega qual o curso de maior duração da ListaDeCursos. */
	    dadoNaPosicao(Curso, ListaCursos, Posicao). 


/* Questão 18 ###########################################################
 * Qual a instituição de ensino com maior número de pessoas?
 * 
 * Primeiro pego a instituição de cada pessoa do banco de dados e crio
 *   uma nova lista.
 * Segundo, aplico o predicado listaMaxima que retorna o elemento da lista
 *   com o maior número de ocorrências.
 * */
qualInstituicaoEnsinoComMaisPessoas(Instituicao) :-
	
	findall(Instit, privado_criarListaDeInstituicoes(Instit), Inst),
	listaMaxima(Inst,Instituicao).
	
	privado_criarListaDeInstituicoes(Instituicao) :-
		
		informacoesAcademicas(L),
		dadoNaPosicao(Instituicao, [_|L], 3).


/* Questão 19 ###########################################################
 * Qual a empresa com maior tempo total de serviço? Considere a soma do 
 *   tempo de serviço de cada pessoa do banco de dados que trabalhou nesta 
 *   empresa. 
 *
 * Primeiro, para cada pessoa na lista você pega as empresas que ela trabalhou
 *   e coloca em uma lista NomesDasEmpresas, que não aceita repetição.
 * Segundo, para cada empresa na lista NomesDasEmpresas, você coloca na lista
 *   TemposDasEmpresas e na mesma posição da empresa NomesDasEmpresas, o 
 *   tempo total de trabalho das pessoas naquela empresa.
 * Terceiro, cria um lista com todas as empresas que empatam em ter o maior 
 *   tempo de serviço.
 * Para isso você passa em todas as pessoas do banco de dados e caso essa 
 *   pessoa tenha trabalho na empresa, você coloca o tempo de trabalho dessa 
 *   pessoa na lista TemposDasEmpresas e na posição correspondente a empresa 
 *   em questão. 
 * Isto é, estas listas contém os dados na mesmo posição, por exemplo, o tempo
 *   de trabalho da empresa 1 está na posição 1 da lista TemposDasEmpresas e o 
 *   nome da empresa 1 está na posição 1 da lista NomesDasEmpresas.
 * */
maiorTempoDeServico(Empresas) :-
	
	findall(Empresa, maiorTempoDeServicoAll(Empresa), Empresas).

maiorTempoDeServicoAll(Empresa) :-
	
	/* Passo 1, colocar o nome de todas as empresas em uma lista ordenada
	 *   NomesDasEmpresas e sem repetição. */
	privado_CarregaLista(NomesDasEmpresas),
	
	/* Passo 2, chamo um predicado que recursivamente, pega a cabeça da lista 
	 *   NomesDasEmpresas e chama o predicado privado_TempoTotalDeTrabalho e 
	 *   salva o TempoTotalDeTrabalho da empresa cabeça da lista
	 *   NomesDasEmpresas na lista variável TemposDasEmpresas. */
	privado_CarregarTempoDeTrabalho(NomesDasEmpresas, TemposDasEmpresas),
	
	/* Descobre qual a posição da empresa com maior tempo. */
	posicaoDoMaior(TemposDasEmpresas, Posicao),
	
    /* Pega o nome da empresa com maior tempo de trabalho. */
    dadoNaPosicao(Empresa, NomesDasEmpresas, Posicao).
    
/* Pega a lista NomesDasEmpresas_Interno, e chama o predicado 
 *   privado_CarregarTempoDeTrabalho.
 * */
privado_CarregarTempoDeTrabalho(ListaNomes, ListaTempos) :-

	/* Chama o predicado que realizada a recursão. */
	privado_CarregarTempoDeTrabalho(ListaNomes, [], ListaTempos).

privado_CarregarTempoDeTrabalho([], Cauda, ListaTempos) :-
	
	/* Salva a lista TemposDasEmpresas. */
	copy_term(Cauda, ListaTempos).

privado_CarregarTempoDeTrabalho(Lista, ListaTempoEntrada, ListaTempos) :-
	
	/* Divide a Lista entre Cabeca e Cauda. */
	dividirLista(Lista, 1, CabecaLista, Cauda), 
	
	/* Transforma a lista de um elemento CabecaLista em um elemento 
	 *   Cabeca. */
	dadoNaPosicao(Cabeca, CabecaLista, 0), 
	
	/* Chama o predicado privado_TempoTotalDeTrabalho. */
	privado_TempoTotalDeTrabalho(Cabeca, Tempo), 
	
	/* Adiciona na ListaTempo o tempo da empresa em Cabeca. */
	inseridoNoFinal(Tempo, ListaTempoEntrada, ListaTempoSaida), 
	
	/* Chama recursivamente este predicado para processar o resto da 
	 *   lista. */
	privado_CarregarTempoDeTrabalho(Cauda, ListaTempoSaida, ListaTempos).

/* Dado o NomeDaEmpresa, calculo o tempo total.
 * */
privado_TempoTotalDeTrabalho(NomeDaEmpresa, TempoTotal) :-
	
	/* Faz todas as requisições ';' para o predicado 
	 *   privado_QualTempoDeTrabalho e cria uma lista de TemposTrabalho. */
	findall(Tempo, privado_QualTempoDeTrabalho(NomeDaEmpresa, Tempo), 
	        TemposTrabalho),
	        
	/* Faz a soma dos TemposTrabalho e retorna na variável TempoTotal. */
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
		
		/* Primeiro, carrego a lista de informacoesProfissionais. */
		informacoesProfissionais(L),
		
		/* Segundo, pego o nome da empresa. */
		dadoNaPosicao(Nome, L, 1),
		
		/* Terceiro, verifico se o Nome carregado é o NomeDaEmpresa que 
		 * queremos. */
		(NomeDaEmpresa = Nome ->
		
			/* Quarto, carregamos o tempo de trabalho da pessoa na variável 
			 * TempoDeTrabalho. */
			dadoNaPosicao(TempoInicial, L, 3),
			dadoNaPosicao(TempoFinal,L,4),
			TempoDeTrabalho is TempoFinal - TempoInicial
		;
			/* Ajusto o valor padrão de TempoDeTrabalho. */
			TempoDeTrabalho is 0
		).

/* Cria um lista contendo todas as empresas.
 * */
privado_CarregaLista(ListaDeEmpresas) :-
	
	/* Faz todas as requisições ';' para o predicado privado_QualEmpresa. */
	findall(Empresa, privado_QualEmpresa(Empresa), Lista),
	
	/* Retira desta lista todos os repetidos e retorna essa lista em 
	 *   ListaDeEmpresas. */
	sort(Lista, ListaDeEmpresas).

	/* Retorna o Nome da Empresa armazenada no predicado 
	 *   informacoesProfissionais. A cada vez que se faz um requisição ';' 
	 *   a este predicado, ele retorna a próxima empresa, até não existirem 
	 *   mais informacoesProfissionais.
	 * */
	privado_QualEmpresa(NomeDaEmpresa) :-
		/* Primeiro carrego a lista de informacoesProfissionais. */
		informacoesProfissionais(L),
		/* Segundo pego o nome da empresa. */
		dadoNaPosicao(NomeDaEmpresa, L, 1).

/* Questão 20 ############ TEMPO DE IMPLEMENTAÇÃO: 8 HORAS! ################
 * Qual a pessoa que mais tempo ficou em um cargo e qual e este cargo? 
 * Exiba seu currículo.
 *
 * 1) Faço uma lista de pessoas.
 * 2) Para cada pessoa na lista, crio uma lista de nomes de cargos que ela  
 *   tenha e outra lista com os tempos dos cargos, correspondente a lista de 
 *   nomes.
 * 3) Pego a posição do maior cargo dela e retorno o nome do cargo que se 
 *   encontra na posição do maior cargo.
 * 4) Crio uma lista com os nomes dos maiores cargos de todas as pessoas e 
 *   outra lista com os tempos dos maiores cargos das pessoas.
 * 5) Descubro qual a posição da pessoa que possui o maior cargo e retorno o 
 *   nome do cargo na posição de maior na lista de nomes dos maiores cargos, 
 *   e retorno o nome dessa pessoa na lista NomesDasPessoas.
 * 6) Refaço a consulta e crio uma lista de Pessoas e Cargos. Isso tem que 
 *   acontecer caso haja empate, isto é, mais de uma pessoa tenha o mesmo 
 *   maior tempo em um cargo. 
 *   O resultado de retorno do predicado será duas listas com correspondência
 *   um-para-um, isto é, o nome na posição 1, corresponde a listas de cargos
 *   que essa pessoa pode ter, e se trata de uma lista por que a pessoa pode 
 *   ter de todos, os maiores cargos onde eles empatam em tempo.
 * 7) Imprimo o currículo da lista de Pessoas que mais ficaram em um cargo. 
 * */
maisTempoEmUmCargo(Pessoas, Cargos) :-
	
	/*6) Refaço a consulta e crio uma lista de Pessoas e Cargos. Isso tem que 
	 *   aconteder caso haja empate, isto é, mais de uma pessoa tenha o mesmo 
	 *   maior tempo em um cargo.*/
	findall(Pessoa, privado_MaisTempoEmUmCargo(Pessoa, _), Pessoas),
	findall(Cargo, privado_MaisTempoEmUmCargo(_, Cargo), Cargos),
	
	/* 7) Imprimo o currículo da lista de Pessoas que mais ficaram em um 
	 *   cargo. */
	imprimirCurriculo(Pessoas),
	
	/* Encerra a execução */
	!.

privado_MaisTempoEmUmCargo(Pessoa, Cargo) :-
	/* 1) Faço uma lista de pessoas. */
	privado_CarregaListaNomes(NomesDasPessoas),
	
	/* 4) Com todos os NomesDasPessoas, crio a lista NomesDosMaioresCargos de 
	 *   todas as pessoas e outra lista TemposDosMaioresCargos de todas as 
	 *   pessoas. */
	privado_CarregarMaioresCargos(NomesDasPessoas, NomesDosMaioresCargos, 
													TemposDosMaioresCargos),
													
	/* 5) Descubro qual a posição da pessoa que possui o maior cargo e retorno 
	 *   o nome do cargo na posição de maior na lista de NomesDosMaioresCargos
	 *   e retorno o nome dessa pessoa na lista NomesDasPessoas. */
	posicaoDoMaior(TemposDosMaioresCargos, Posicao),
    dadoNaPosicao(Cargo, NomesDosMaioresCargos, Posicao),
    dadoNaPosicao(Pessoa, NomesDasPessoas, Posicao).
	
/* ######################## Predicados Auxiliares ###################### */
/* Recebendo todos os NomesDasPessoas como uma lista, cria a lista 
 *   NomesDosMaioresCargos de todas as pessoas e outra lista 
 *   TemposDosMaioresCargos de todas as pessoas. 
 * */
privado_CarregarMaioresCargos(NomesDasPessoas, NomesDosMaioresCargos, 
											TemposDosMaioresCargos) :-
	/* Chama o predicado que realizada a recursão.*/
	privado_CarregarMaioresCargos(NomesDasPessoas, [], [], 
						NomesDosMaioresCargos, TemposDosMaioresCargos).

/* Retorna a lista contendo o resultado acumulado na recursão. */ 
privado_CarregarMaioresCargos([], NomesDosMaioresCargos, 
		TemposDosMaioresCargos, NomesDosMaioresCargos_Interno, 
										TemposDosMaioresCargos_Interno) :-	
											
	copy_term(NomesDosMaioresCargos, NomesDosMaioresCargos_Interno), 
	copy_term(TemposDosMaioresCargos, TemposDosMaioresCargos_Interno).

/* Corpo principal da recursão que interege com todos os elementos de 
 *   NomesDasPessoas_Interno, e coloca o resultado da iteração na 
 *   ListaEntradaCargo e ListaEntradaTempo.
 * */
privado_CarregarMaioresCargos(NomesDasPessoas_Interno, 
ListaEntradaCargo, ListaEntradaTempo, NomesDosMaioresCargos_Interno, 
										TemposDosMaioresCargos_Interno) :-
	/* Pega o primeiro nome da lista NomesDasPessoas_Interno e coloca 
	 *   o restante da lista em Cauda. */
	dividirLista(NomesDasPessoas_Interno, 1, CabecaLista, RestoDaLista), 
	
	/* Transforma a lista de 1 elemento CabecaLista em um elemento 
	 *   NomeDaPessoa. */
	dadoNaPosicao(NomeDaPessoa, CabecaLista, 0), 
	
	/* Calcula qual o MaiorCargoNome e MaiorCargoTempo do NomeDaPessoa. */
	privado_QualTodosMaiorCargoDe(NomeDaPessoa, MaiorCargoNome, 
														MaiorCargoTempo),
	
	/* Adiciona na ListaSaidaCargo o nome do maior cargo do NomeDaPessoa. */
	inseridoNoFinal(MaiorCargoNome, ListaEntradaCargo, ListaSaidaCargo), 
	
	/* Adiciona na ListaSaidaTempo o tempo do maior cargo de 
	 *   NomeDaPessoa. */
	inseridoNoFinal(MaiorCargoTempo, ListaEntradaTempo, ListaSaidaTempo), 
	
	/* Chama recursivamente este predicado para processar o resto da 
	 *   lista. */
	privado_CarregarMaioresCargos(RestoDaLista, ListaSaidaCargo, 
	ListaSaidaTempo, NomesDosMaioresCargos_Interno, 
										TemposDosMaioresCargos_Interno).

/* 2) Para cada pessoa na lista: */
/* Dado o nome de uma pessoa NomeDaPessoa, retorna uma lista contendo 
 *   os MaiorCargoNome_Interno's (quando há empate de maiores) e o 
 *   MaiorCargoTempo_Interno. */
privado_QualTodosMaiorCargoDe(NomeDaPessoa, MaiorCargoNome_Interno, 
											MaiorCargoTempo_Interno) :-
	/* Realiza todas as requisições ';' para privado_QualMaiorCargoDe, e
	 *   cria uma lista contendo todos os cargos que tem o maior tempo, 
	 *   isto é, quando ha empate de tempo. */
	 findall(Maior, privado_QualMaiorCargoDe(NomeDaPessoa, Maior, _), 
	 											MaiorCargoNome_Interno),
	 privado_QualMaiorCargoDe(NomeDaPessoa, _, MaiorCargoTempo_Interno),
	 /* Força encerra as requisições desta cláusula. Isto é necessário 
	  *   por que a requisições depois do findall acima, por padrão são  
	  *   feitas várias vezes até se encerrar os maiores cargos. */
	 !.

/* Dado o nome de uma pessoa NomeDaPessoa, retorna MaiorCargoNome_Interno, 
 *   e o MaiorCargoTempo_Interno. */
privado_QualMaiorCargoDe(NomeDaPessoa, MaiorCargoNome_Interno, 
											MaiorCargoTempo_Interno) :-
	/* Crio uma lista de nomes de cargos que ela tenha, ordenada e 
	 *   sem repetição. */
	privado_CriarListaCargos(NomeDaPessoa, NomesDosCargos),
	
	/* Crio uma lista com os TemposDosCargos, correspondente a lista de 
	 *   NomesDosCargos de um NomeDaPessoa. */
	privado_CarregarTemposDosCargos(NomeDaPessoa, NomesDosCargos, 
														TemposDosCargos),
	/* 3) Pego a posição do maior cargo dela e retorno o 
	 *   MaiorCargoNome_Interno e o MaiorCargoTempo_Interno, que se 
	 *   encontram na posição do maior cargo.
	 * */
	posicaoDoMaior(TemposDosCargos, Posicao),
    dadoNaPosicao(MaiorCargoNome_Interno, NomesDosCargos, Posicao),
    dadoNaPosicao(MaiorCargoTempo_Interno, TemposDosCargos, Posicao).

	/* Carrego a lista NomesDosCargos_Interno e TemposDosCargos_Interno 
	 *    de NomeDaPessoa_Interno.
	 * */
	privado_CarregarTemposDosCargos(NomeDaPessoa_Interno, 
					NomesDosCargos_Interno, TemposDosCargos_Interno) :-
		/* Chama o predicado que realizada a recursão. */
		privado_CarregarTemposDosCargos(NomeDaPessoa_Interno, 
					NomesDosCargos_Interno, [], TemposDosCargos_Interno).
	
	/* Retorna a lista contendo o resultado acumulado na recursão.
	 * */
	privado_CarregarTemposDosCargos(_, [], TemposDosCargos, 
												TemposDosCargos_Interno) :-	
		copy_term(TemposDosCargos, TemposDosCargos_Interno).
	
	/* Corpo principal da recursão que interege com todos os elementos de 
	 *   NomesDosCargos_Interno, de NomeDaPessoa_Interno e coloca o 
	 *   resultado da iteração em ListaSaida.
	 * A lista final conterá o tempo total de um dado cargo, mesmo que 
	 *   esse cargo tenha ocorrido em diversos trabalhos, por exemplo, 
	 *   trabalhou 5 anos como Professor na escola x, e mais 8 anos na 
	 *   escola y.
	 * */
	privado_CarregarTemposDosCargos(NomeDaPessoa_Interno, 
		NomesDosCargos_Interno, ListaEntrada, TemposDosCargos_Interno) :-
		/* Pega o primeiro cargo de NomesDosCargos_Interno e coloca o 
		 *  restante dos cargos em Restante. */
		dividirLista(NomesDosCargos_Interno, 1, CabecaLista, Restante), 
		
		/* Transforma a lista de 1 elemento CabecaLista em um elemento 
		 *   Cabeca. */
		dadoNaPosicao(NomeDoCargo, CabecaLista, 0), 
		
		/* Calculo qual o Tempo total do NomeDoCargo de 
		 *   NomeDaPessoa_Interno */
		privado_TempoTotalDoCargo(NomeDaPessoa_Interno, NomeDoCargo, Tempo), 
		
		/* Adiciona na ListaSaida o tempo do cargo em NomeDoCargo. */
		inseridoNoFinal(Tempo, ListaEntrada, ListaSaida), 
		
		/* Chama recursivamente este predicado para processar o resto da 
		 *   lista. */
		privado_CarregarTemposDosCargos(NomeDaPessoa_Interno, Restante, 
									ListaSaida, TemposDosCargos_Interno).
	
	/* Dado o NomeDaPessoa e o NomeDoCargo, calculo o tempo total desse 
	 *   cargo.
	 * */
	privado_TempoTotalDoCargo(NomeDaPessoa, NomeDoCargo, TempoTotal) :-
		/* Faz todas as requisições ';' para o predicado 
		 *   privado_TempoDoCargo e cria uma lista de TemposTrabalho.
		 * */
		findall(Tempo, privado_TempoDoCargo(NomeDaPessoa, 
									NomeDoCargo, Tempo), TemposDoCargo),
		/* Faz a soma dos TemposTrabalho e retorna na variável TempoTotal.
		 * */
		somaDosElementos(TemposDoCargo, TempoTotal).
		
		/* Retorna o tempo de cargo em uma pessoa NomeDaPessoa_Interno, 
		 *   NomeDoCargo_Interno armazenada no predicado 
		 *   informacoesProfissionais. 
		 * A cada vez que se faz um requisição ';' a este predicado, ele 
		 *   retorna o TempoDeCargo de NomeDaPessoa_Interno e 
		 *   NomeDoCargo_Interno, caso a empresa pega no predicado 
		 *   informacoesProfissionais não seja NomeDaPessoa_Interno e 
		 *   NomeDoCargo_Interno retorna 0 em TempoDeCargo, caso contrário, 
		 *   retorna o TempoDeCargo.
		 * */
		privado_TempoDoCargo(NomeDaPessoa_Interno, NomeDoCargo_Interno, 
														TempoDeCargo) :-
			/* Primeiro, carrego a lista de informacoesProfissionais. */
			informacoesProfissionais(L), 
			
			/* Segundo, pego o nome da pessoa e do cargo. 
			 * */
			dadoNaPosicao(Nome, L, 0), 
			dadoNaPosicao(Cargo, L, 2), 
			/* Terceiro, verifico se o Nome e o Cargo carregado é o 
			 * NomeDaPessoa_Interno e NomeDoCargo_Interno que queremos. 
			 * */
			(NomeDaPessoa_Interno = Nome, Cargo = NomeDoCargo_Interno ->
				/* Quarto, carregamos o TempoDeCargo da pessoa. */
				dadoNaPosicao(TempoInicial, L, 3),
				dadoNaPosicao(TempoFinal,L,4),
				TempoDeCargo is TempoFinal - TempoInicial
			;
				/* Ajusto o valor padrão de TempoDeCargo. */
				TempoDeCargo is 0
			).
	
	/* Para uma pessoa Nome, cria uma lista de nomes de Cargos que ela 
	 *   tenha, ordenado e sem repetição.
	 * */
	privado_CriarListaCargos(Nome_Interno, Cargos_Interno) :-
		findall(Cargo, privado_CarregarCargo(Nome_Interno, Cargo), Cargos),
		sort(Cargos, Cargos_Interno).
		
		/* Para um dado Nome, retorna o Cargo dele.
		 * */
		privado_CarregarCargo(NomeDaPessoa, CargoDaPessoa) :-
			informacoesProfissionais(L),
			dadoNaPosicao(Nome, L, 0),
			Nome == NomeDaPessoa, 
			dadoNaPosicao(CargoDaPessoa, L, 2).

/* Cria um lista contendo todas as pessoas.
 * */
privado_CarregaListaNomes(NomesDasPessoas) :- 
	/* Faz todas as requisições ';' para o predicado privado_QualPessoa.
	 * */
	findall(Nome, privado_QualPessoa(Nome), Lista),
	/* Retira desta lista todos os repetidos e retorna essa lista em 
	 *   NomesDasPessoas.
	 * */
	sort(Lista, NomesDasPessoas).
	
	/* Retorna o Nome da Empresa armazenada no predicado 
	 *   informacoesProfissionais. A cada vez que se faz um requisição ';' 
	 *   a este predicado, ele retorna a próxima empresa, até não existirem 
	 *   mais informacoesProfissionais.
	 * */
	privado_QualPessoa(NomeDaPessoa) :-
		/* Primeiro carrego a lista de informacoesProfissionais. 
		 * */
		informacoesPessoais(L),
		/* Segundo pego o nome da empresa. 
		 * */
		dadoNaPosicao(NomeDaPessoa, L, 0).


/* Questão 21 ###########################################################
 * Qual a pessoa mais citada como referência? Exiba seu currículo.
 * 
 * O algoritmo cria, inicialmente, duas listas de listas, uma com todas
 *   as referências acadêmicas e outra com todas as referências
 *   profissionais.
 * Após, cada lista é transformada em uma lista simples usando append,
 *   e então são concatenadas em apenas uma única lista.
 * Por fim, esta lista é passada pelo predicado listaMaxima que nos retorna
 *   o elemento de maior ocorrência.
 * */
qualReferenciaMaisCitada(Referencia) :-
	findall(Ref, privado_PegarReferenciasAcademicas(Ref), Lista),
	findall(Ref2, privado_PegarReferenciasProfissionais(Ref2), Lista2),
	append(Lista, ListaRef),
	append(Lista2, ListaRef2),
	concatenadas(ListaRef, ListaRef2, ListaRefCompleta),
	listaMaxima(ListaRefCompleta,Referencia_T),
	primeiro(Referencia_T, Referencia).

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


/* Questão 22 ###############################################################
 * Qual a pessoa com maior quantidade de colegas referenciados (de curso
 *   ou de trabalho)? Exiba seu currículo.
 *
 * Este predicado não recebe entradas.
 * Ele criará uma lista com todas as pessoas do banco de dados e a
 *   seguir criará outra lista de valores numéricos que correspondem,
 *   baseado na sua posição, à quantidade de referências da pessoa que se
 *   encontra na lista de pessoas, na mesma posição que o valor numérico.
 * Após, é encontrado o maior valor na lista numérica, encontrado o seu
 *   índice e retornado o nome da pessoa de mesmo índice na lista de 
 *   pessoas.
 * */
qualPessoaComMaiorQtdReferencias(Resposta) :-
	privado_MakeList(Pessoas),
	privado_CarregarReferencias(Pessoas),
	nb_getval(qtdReferencias, ListaRef),
	maiorElemento(ListaRef, Quantidade),
	indiceDoElemento(ListaRef, Quantidade, I),
	dadoNaPosicao(Resposta, Pessoas, I),
	imprimirCurriculo([Resposta]).

	/* Predicado que cria uma lista com todas as pessoas do banco de dados. */
	privado_MakeList(Pessoas) :-
		findall(Nome, privado_Pessoas(Nome), Lista),
		sort(Lista, Pessoas).
			
	/* Predicado auxiliar para privado_MakeList(Pessoas); este adquire o
	 *   nome da pessoa. */
	privado_Pessoas(Nomes) :-
		informacoesAcademicas(L),
		dadoNaPosicao(Nomes, L, 0).
		
	/* Pega a lista Referencias_Interno, e chama o predicado 
	 *   privado_CarregarReferencias. */
	privado_CarregarReferencias(Referencias_Interno) :-
		
		/* Chama o predicado que realizada a recursão. */
		privado_CarregarReferencias(Referencias_Interno, []).
		 
		/* Salva a lista qtdReferencias. */
		privado_CarregarReferencias([], Cauda) :-
			nb_setval(qtdReferencias, Cauda),
			!.
		
	/* Predicado que criará a lista com a quantidade de referencias de cada
	 *   pessoa. */
	privado_CarregarReferencias(Lista, Lista2) :-
		
		/* Divido a lista em cabeça(contendo o nome da pessoa) e 
		 *   cauda(resto). */
		dividirLista(Lista, 1, CabecaLista, Cauda),
		
		/* Pego o nome da pessoa. */
		dadoNaPosicao(Cabeca, CabecaLista, 0),
		
		/* Calculo o seu total de referências profissionais. */
		privado_TotalReferenciasP(Cabeca, Aux1),
		
		/* Calculo o seu total de referências academicas. */
		privado_TotalReferenciasA(Cabeca, Aux2),
		
		/* Calculo o seu total de referências. */
		Ref is Aux1 + Aux2,
		
		/* Insiro na lista de referências. */
		inseridoNoFinal(Ref, Lista2, ListaRef),
		
		/* Realizo a recursão. */
		privado_CarregarReferencias(Cauda, ListaRef).
		
	/* Predicado que calcula o total de referencias profissionais de uma dada
	 *   pessoa.
	 * */	
	privado_TotalReferenciasP(NomePessoa, TotalRef) :-
		
		/* Faz todas as requisições ';' para o predicado 
		 *   privado_QtdReferenciasP e cria uma lista de TotalRef. */
		findall(Qtd, privado_QtdReferenciasP(NomePessoa, Qtd), List),
		somaDosElementos(List, TotalRef).
		
	/* Retorna a quantidade de referências profissionais de uma pessoa do
	 *   banco de dados em informacoesProfissionais. 
	 * A cada vez que se faz um requisição ';' a este predicado, ele 
	 *   retorna a quantidade de referências caso Pessoa(nome) seja igual
	 *   ao Nome encontrado em informacoesProfissionais, caso não seja, é 
	 *   retornado 0 em QtdRef */
	privado_QtdReferenciasP(Pessoa, QtdRef) :-
		
		/* Primeiro carrego uma lista de informacoesProfissionais. */
		informacoesProfissionais(L),
		
		/* Segundo, pego um Nome. */
		dadoNaPosicao(Nome, L, 0),
		
		/* Terceiro, confiro se Pessoa e Nome são iguais. */
		(Pessoa = Nome ->
		
			/* Caso sim, divido a lista para pegar apenas suas referências e 
			 *   calculo o seu tamanho. */
			dividirLista(L,5,_,L2),
			length(L2,N),
			QtdRef is N
		;
			/* Caso não, retorno 0. */
			QtdRef is 0
		).
		
	/* Predicado que calcula o total de referencias academicas de uma dada
	 *   pessoa. */
	privado_TotalReferenciasA(NomePessoa, TotalRef) :-
		/* Faz todas as requisições ';' para o predicado 
		 *   privado_QtdReferenciasA e cria uma lista de TotalRef. */
		findall(Qtd, privado_QtdReferenciasA(NomePessoa, Qtd), List),
		somaDosElementos(List, TotalRef).
	
	/* Retorna a quantidade de referências acadêmicas de uma pessoa do
	 *   banco de dados em informacoesAcademicas. 
	 * A cada vez que se faz um requisição ';' a este predicado, ele 
	 *   retorna a quantidade de referências caso Pessoa(nome) seja igual
	 *   ao Nome encontrado em informacoesAcademicas, caso não seja, é 
	 *   retornado 0 em QtdRef */	
	privado_QtdReferenciasA(Pessoa, QtdRef) :-
		
		/* Primeiro carrego uma lista de informacoesAcademicas. */
		informacoesAcademicas(L),
		
		/* Segundo, pego um Nome. */
		dadoNaPosicao(Nome, L, 0),
		
		/* Terceiro, confiro se Pessoa e Nome são iguais. */
		(Pessoa = Nome ->
		
			/* Caso sim, divido a lista para pegar apenas suas referências e 
			 *   calculo o seu tamanho. */
			dividirLista(L,6,_,L2),
			length(L2,N),
			QtdRef is N
		;
			/* Caso não, retorno 0. */
			QtdRef is 0
		).


/* Questão 23 ################################################################
 * Considerando que todas as pessoas, que estudaram em uma dada instituição 
 *   ou trabalharam em uma dada empresa, tem um certo relacionamento. Qual 
 *   é a quantidade de tais relacionamentos para uma dada pessoa?
 * 
 * Dado uma pessoa. 
 * 1) Calcula a quantidade de relacionamentos de trabalho.
 * 2) Calcula a quantidade de relacionamentos de faculdade.
 * 3) Soma os relacionamentos e retorna a Quantidade.
 *
 * 1) Pego o nome da pessoa, descubro em quais empresas/instituições ele 
 *  trabalhou/estudou.
 * 2) Para cada empresa/instituição que ela trabalhou/estudou, passo em todas 
 *   as outras pessoas e verifico se elas também trabalharam/estudaram nessa 
 *   empresa/instituição e somo o total de pessoas que trabalharam nessas 
 *   empresas/instituições, sem recontar a pessoa fornecida.
 * */
quantidadeDeRelacionamentos(Nome, Quantidade) :-
	
	/* Pego o nome da pessoa, descubro em quais empresas ele trabalha. */
	privado_RelacionamentosDeTrabalho(Nome, Empresas), 
	
	/* Descubro em quais Instituições ela estudou. */
	privado_RelacionamentosDeInstituicao(Nome, Instituicao),
	
	/* Calcula quantas pessoas trabalham nas Empresas da lista. */
	privado_RelacionamentosQuantosTrabalham(Nome, Empresas, 
														QuantidadeEmpresas),
														
	/* Calcula quantas pessoas esturam nas Instituições da lista. */
	privado_RelacionamentosQuantosCursaram(Nome, Instituicao, 
														QuantidadeInstituicao),
														
	/* Calcula a quantidade total de relacionamentos. */
	Quantidade is QuantidadeInstituicao + QuantidadeEmpresas.
	
	/* Recebe uma lista de Instituições e retorna a Quantidade de pessoas que 
	 *   estudaram nelas.
	 * */
	privado_RelacionamentosQuantosCursaram(Nome, Instituicoes, Quantidade ) :-
		
		privado_RelacionamentosQuantosCursaram(Nome, Instituicoes, 0, 
																Quantidade ),
		!.
		
	/* Funcioamento interno de privado_RelacionamentosQuantosTrabalham */
	privado_RelacionamentosQuantosCursaram(_, [], QuantidadeTemp, 
																Quantidade ) :-
		
		copy_term(QuantidadeTemp, Quantidade).
	
	/* Funcioamento interno de privado_RelacionamentosQuantosTrabalham */
	privado_RelacionamentosQuantosCursaram(PessoaAtual, Instituicoes, 
												QuantidadeTemp, Quantidade ) :-

		dividirLista(Instituicoes, 1, InstituicaoTemp, RestoInstituicoes),
		primeiro(InstituicaoTemp, Instituicao),
		privado_QuantosCursaramNesseCurso(PessoaAtual, Instituicao, 
															QuantidadeTemp2),
		NovaQuantidadeTemp is QuantidadeTemp + QuantidadeTemp2,
		privado_RelacionamentosQuantosCursaram(PessoaAtual, RestoInstituicoes, 
											NovaQuantidadeTemp, Quantidade ).
	
	/* Recebe uma Instituicao e retorna Quantidade de pessoas que estudaram, 
	 *   nela descontado a PessoaAtual fornecida como parâmetro.
	 * */
	privado_QuantosCursaramNesseCurso(PessoaAtual, Instituicao, Quantidade) :-
	
		findall(QuantidadeTemp, privado_QuantosCursaramNesseCursoTemp(
					PessoaAtual, Instituicao, QuantidadeTemp), QuantidadeLista ), 
		somaDosElementos(QuantidadeLista, Quantidade).
		
		privado_QuantosCursaramNesseCursoTemp(PessoaAtual, Instituicao, 
																Quantidade) :-
			
			informacoesAcademicas(InformacoesAcademicas),
			dadoNaPosicao(InstituicaoTemp, InformacoesAcademicas, 2),
			dadoNaPosicao(PessoaAtualTemp, InformacoesAcademicas, 0),
			( InstituicaoTemp = Instituicao -> 
				( PessoaAtual \= PessoaAtualTemp ->
					Quantidade is 1
				;
					Quantidade is 0
				)
			;
				Quantidade is 0
			).
	
	/* Recebe um Nome e retorna uma lista de Instituicoes em que a pessoa 
	 *   estudou.
	 * */
	privado_RelacionamentosDeInstituicao(Nome, Instituicoes) :-
		
		/* Faz todas as requisições ';' para o predicado 
		 *   privado_QualInstituicao. */
		findall(Instituicao, privado_QualInstituicao(Nome, Instituicao), 
															InstituicoesTemp),
		
		/* Retira desta lista todos os repetidos e retorna essa lista em 
		 *   Instituicoes. */
		sort(InstituicoesTemp, Instituicoes).
	
		/* Retorna o Nome da Instituicao armazenada no predicado 
		 *   informacoesAcademicas. A cada vez que se faz um requisição ';' 
		 *   a este predicado, ele retorna a próxima Instituicao, até não 
		 *   existirem mais informacoesAcademicas.
		 * */
		privado_QualInstituicao(Nome, NomeDaInstituicao) :-
			informacoesAcademicas(L),
			dadoNaPosicao(NomeTemp, L, 0),
			NomeTemp = Nome,
			dadoNaPosicao(NomeDaInstituicao, L, 2).
	
	/* Recebe uma lista de Empresas e retorna a Quantidade de pessoas que 
	 * Trabalham nessas empresas.
	 * */
	privado_RelacionamentosQuantosTrabalham(Nome, Empresas, Quantidade ) :-
		
		privado_RelacionamentosQuantosTrabalham(Nome, Empresas, 0, 
																Quantidade ),
		!.
		
	/* Funcioamento interno de privado_RelacionamentosQuantosTrabalham */
	privado_RelacionamentosQuantosTrabalham(_, [], QuantidadeTemp, 
																Quantidade ) :-
		
		copy_term(QuantidadeTemp, Quantidade).
	
	/* Funcioamento interno de privado_RelacionamentosQuantosTrabalham */
	privado_RelacionamentosQuantosTrabalham(PessoaAtual, Empresas, 
												QuantidadeTemp, Quantidade ) :-

		dividirLista(Empresas, 1, EmpresaTemp, RestoEmpresas),
		primeiro(EmpresaTemp, Empresa),
		privado_QuantosTrabalhamNessaEmpresa(PessoaAtual, Empresa, 
															QuantidadeTemp2),
		NovaQuantidadeTemp is QuantidadeTemp + QuantidadeTemp2,
		privado_RelacionamentosQuantosTrabalham(PessoaAtual, RestoEmpresas, 
											NovaQuantidadeTemp, Quantidade ).
	
	/* Recebe uma Empresa e retorna Quantidade de pessoas que trabalham nela, 
	 *   descontado a PessoaAtual fornecida como parâmetro.
	 * */
	privado_QuantosTrabalhamNessaEmpresa(PessoaAtual, Empresa, Quantidade) :-
	
		findall(QuantidadeTemp, privado_QuantosTrabalhamNessaEmpresaTemp(
					PessoaAtual, Empresa, QuantidadeTemp), QuantidadeLista ), 
		somaDosElementos(QuantidadeLista, Quantidade).
		
		privado_QuantosTrabalhamNessaEmpresaTemp(PessoaAtual, Empresa, 
																Quantidade) :-
			
			informacoesProfissionais(InformacoesProfissionais),
			dadoNaPosicao(EmpresaTemp, InformacoesProfissionais, 1),
			dadoNaPosicao(PessoaAtualTemp, InformacoesProfissionais, 0),
			( EmpresaTemp = Empresa -> 
				( PessoaAtual \= PessoaAtualTemp ->
					Quantidade is 1
				;
					Quantidade is 0
				)
			;
				Quantidade is 0
			).

	/* Recebe um Nome e retorna uma lista de Empresas em que a pessoa trabalha.
	 * */
	privado_RelacionamentosDeTrabalho(Nome, Empresas) :-
		
		/* Faz todas as requisições ';' para o predicado privado_QualEmpresa. */
		findall(Empresa, privado_QualEmpresa(Nome, Empresa), Empresas_Temp),
		
		/* Retira desta lista todos os repetidos e retorna essa lista em 
		 *   Empresas. */
		sort(Empresas_Temp, Empresas).
	
		/* Retorna o Nome da Empresa armazenada no predicado 
		 *   informacoesProfissionais. A cada vez que se faz um requisição ';' 
		 *   a este predicado, ele retorna a próxima empresa, até não existirem 
		 *   mais informacoesProfissionais.
		 * */
		privado_QualEmpresa(Nome, NomeDaEmpresa) :-
			/* Primeiro carrego a lista de informacoesProfissionais. */
			informacoesProfissionais(L),
			dadoNaPosicao(NomeTemp, L, 0),
			NomeTemp = Nome,
			/* Segundo pego o nome da empresa. */
			dadoNaPosicao(NomeDaEmpresa, L, 1).


/* Questão 24 ##############################################################
 * Qual a pessoa com a maior quantidade de relacionamentos (segundo a 
 *   definição da questão anterior)? Exiba seu currículo.
 * 
 * 1) Crio uma lista contendo o nome de todas as pessoas, sem repetição e 
 *   em ordem.
 * 2) A partir da lista de nomes, crio outra lista contendo a quantidade de 
 *   relacionamentos, onde existe a correspondência 1 para 1 entre a lista de 
 *   nomes e a lista de relacionamentos, isto á, para o nome na posição 1 da 
 *   lista de nomes, existe na lista de relacionamentos na posição 1, a sua 
 *   quantidade de relacionamentos.
 * 3) Encontro a posição do maior elemento na lista de relacionamentos e 
 *   retorno o nome da pessoa correspondente na lista de nomes.
 * 4) Imprimo o currículo na tela.
 * 5) A cada vez pode-se fazer a requisição ‘;’ que encontrar outras pessoas 
 *   com a maior quantidade, caso haja empate.
 * */
maiorQuantidadeDeRelacionamentos(Nome) :-
	
	nomeDeTodasAsPessoas(Nomes),
	privado_QuantidadeRelacionamentos(Nomes, Relacionamentos),
	posicaoDoMaior(Relacionamentos, Posicao),
	dadoNaPosicao(Nome, Nomes, Posicao),
	imprimirCurriculo([Nome]).
	
	/* Retorna uma lista Relacionamentos contendo em ordem a quantidade de 
	 *   relacionamentos que cada pessoa da lista Nomes tem.
	 * */
	privado_QuantidadeRelacionamentos(Nomes, Relacionamentos) :-
		
		privado_QuantidadeRelacionamentos(Nomes, [], Relacionamentos).
	
	privado_QuantidadeRelacionamentos([], Temp, Relacionamentos) :-
		
		copy_term(Temp, Relacionamentos).
		
	privado_QuantidadeRelacionamentos(Nomes, Temp, Relacionamentos) :-
		
		dividirLista(Nomes, 1, NomeTemp, RestoNomes),
		primeiro(NomeTemp, Nome),
		quantidadeDeRelacionamentos(Nome, Quantidade),
		inseridoNoFinal(Quantidade, Temp, TempSaida),
		privado_QuantidadeRelacionamentos(RestoNomes, TempSaida, 
															Relacionamentos).


:-begin_tests(maiorQuantidadeDeRelacionamentos).

test(maiorQuantidadeDeRelacionamentos) :-
	
	findall(A, maiorQuantidadeDeRelacionamentos(A), All),
	primeiro(All, A2), 
    assertion(A2 == 'Stiven Stronger').
    
test(privado_QuantidadeRelacionamentos) :-
	
	findall(A, privado_QuantidadeRelacionamentos(['Stiven Stronger'], A), 
															Relacionamentos),
	primeiro(Relacionamentos, R),
    assertion(R =:= 8).
    
:-end_tests(maiorQuantidadeDeRelacionamentos).


/* Questão 25 ###############################################################
 * Crie uma pergunta qualquer (ainda não feita) que envolva a determinação 
 *   de máximo ou de mínimo de um conjunto numérico.
 * 
 * Qual a idade da pessoa mais velha?
 * 
 * 1) Crio uma lista contendo a idade de todas as pessoas.
 * 2) Pego qual a maior idade dessa lista e retorno em Idade
 */
qualMaiorIdade(Idade) :-
	
	findall( IdadeTemp, privado_QualMaiorIdade(IdadeTemp), Idades),
	maiorElemento(Idades, Idade),
	!.
	
	privado_QualMaiorIdade(Idade) :-
		informacoesPessoais(L),
		dadoNaPosicao(DataDeNascimento, L, 1),
		privado_CalcularIdade(DataDeNascimento, Idade).


:-begin_tests(qualMaiorIdade).

test(qualMaiorIdade) :-
	
    A is 2^3,
    qualMaiorIdade(Idade),
    assertion( Idade == 44 ),
    assertion(A == 8).
    
:-end_tests(qualMaiorIdade).


/* ###########################################################################
 * Imprime o currículo na tela, dado uma lista com nomes de Pessas na base de 
 *   dados. Caso não seja entradado alguém, imprime uma mensagem de erro na
 *   tela.
 * */
imprimirCurriculo(Pessoas_Interno) :-
	/* Chama a recursão que imprime o currículo. */
	privado_ImprimirCurriculo(Pessoas_Interno, 1).
	
	/* Encerra a recursão quando termina a lista Pessoas_Interno. */
	privado_ImprimirCurriculo([], _).
	
	/* Imprime o currículo de acordo com o seu numero */
	privado_ImprimirCurriculo(Pessoas_Interno, CurriculoNumero) :-
		
		/* Pega a cabeça da lista como uma lista e a lista Cauda */
		dividirLista(Pessoas_Interno, 1, CabecaLista, RestoDasPessoas), 
		
		/* Transforma a lista CabecaLista no elemento Nome da pessoa. */
		dadoNaPosicao(Nome, CabecaLista, 0), 
		
		/* Carrega todas as imformações pessoais. */
		informacoesPessoais(InformacoesPessoais),
		dadoNaPosicao(NomeDaPessoa, InformacoesPessoais, 0),
		NomeDaPessoa == Nome,
		
		/* Imprime na tela a apresentação e o nome da pessoa */
		dadoNaPosicao(DataDeNascimento, InformacoesPessoais, 1), 
		write('\nCurriculo '), write(CurriculoNumero),
		write(' ###########################################################'),
		write('\nNome: '), write(Nome),
		
		/* Imprime a DataDeNascimento no formato padrão do swi-prolog como 
		 *   Dia/Mês/Ano. */
		privado_ImprimirCurriculoNascimento(DataDeNascimento),
		
		/* Imprime qual a cidade da pessoa. */
		dadoNaPosicao(CidadeDaPessoa, InformacoesPessoais, 2),
		write('\nCidade: '), write(CidadeDaPessoa),
		
		/* Imprime o telefone da pessoa. */
		qualTelefone(Nome, Telefones),
		write('\nTelefone(s): '), write(Telefones),
		
		/* Imprime todas as formações acadêmicas de todos os cursos. */
		findall(_, privado_ImprimirCurriculoAcademicas(Nome), _ ),
		
		/* Imprime todas as formações profisionais de todos os trabalhos. */
		findall(_, privado_ImprimirCurriculoProfissionais(Nome), _ ),
		
		/* Imprime um separador */
		write('\n '),
		
		/* Ajusta o novo número do currículo */
		NovoCurriculoNumero is CurriculoNumero + 1,
		
		/* Chama a recursão que imprime os outros currilos */
		privado_ImprimirCurriculo(RestoDasPessoas, NovoCurriculoNumero).
	
	/* Imprime a DataDeNascimento que deve estar no formato padrão do 
	 *   swi-prolog date como Dia/Mês/Ano..
	 * */
	privado_ImprimirCurriculoNascimento(DataDeNascimento) :-
		date_time_stamp(DataDeNascimento, Data), 
	    convert_time(Data, Ano, Mes, Dia,_,_,_,_),
	    write('\nData de Nascimento: '), 
	    write(Dia), write('/'), write(Mes), write('/'), write(Ano).
	
	/* Imprime todas as formações academicas de uma pessoa.
	 * */
	privado_ImprimirCurriculoAcademicas(Nome) :-
		/* Carrega as informações acadêmicas. */
		informacoesAcademicas(InformacoesAcademicas),
		dadoNaPosicao(NomeDaPessoa, InformacoesAcademicas, 0),
		NomeDaPessoa == Nome,
		
		/* Imprime um separador */
		write('\n\n$$$$$$$$$$$$$$$$$$$'),
		
		/* Imprime o nome do curso. */
		dadoNaPosicao(Curso, InformacoesAcademicas, 1),
		write('\nFormado no Curso de: '), write(Curso),
		
		/* Imprime o Nome da Instituição. */
		dadoNaPosicao(Instituicao, InformacoesAcademicas, 2),
		write('\nNa instituicao: '), write(Instituicao),
		
		/* Imprime nome completo do orientador. */
		dadoNaPosicao(Orientador, InformacoesAcademicas, 3),
		write('\nNome do Orientador: '), write(Orientador),
		
		/* Imprime nome completo de colegas de curso. */
		dividirLista( InformacoesAcademicas, 6, _, Referencias ),
		write('\nCom as Referencias Academicas: '), write(Referencias),
		
		/* Imprime o ano de ingresso no curso. */
		dadoNaPosicao(Ingresso, InformacoesAcademicas, 4),
		write('\nIngressou no Ano de: '), write(Ingresso),
		
		/* Imprime o ano de termino do curso. */
		dadoNaPosicao(Termino, InformacoesAcademicas, 5),
		write('\nFormou-se no Ano de: '), write(Termino).
	
	
	/* Imprime todas as formações academicas de uma pessoa.
	 * */
	privado_ImprimirCurriculoProfissionais(Nome) :-
		/* Carrega as informações profissionais. */
		informacoesProfissionais(InformacoesProfissionais),
		dadoNaPosicao(NomeDaPessoa, InformacoesProfissionais, 0),
		NomeDaPessoa == Nome,
		
		/* Imprime um separador */
		write('\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'),
		
		/* Imprime o nome da empresa. */
		dadoNaPosicao(Empresa, InformacoesProfissionais, 1),
		write('\nTrabalhou da Empresa: '), write(Empresa),
		
		/* Imprime o nome do cargo. */
		dadoNaPosicao(Cargo, InformacoesProfissionais, 2),
		write('\nNo Cargo de: '), write(Cargo),
		
		/* Imprime nome completo de colegas de trabalhos. */
		dividirLista( InformacoesProfissionais, 5, _, Referencias ),
		write('\nCom as Referencias Profissionais: '), write(Referencias),
		
		/* Imprime o ano de ingresso no cargo. */
		dadoNaPosicao(Ingresso, InformacoesProfissionais, 3),
		write('\nContratado no Ano de: '), write(Ingresso),
		
		/* Imprime o ano de termino do cargo. */
		dadoNaPosicao(Termino, InformacoesProfissionais, 4),
		write('\nDesligado no Ano de: '), write(Termino).


:-begin_tests(imprimirCurriculo).

test(imprimirCurriculo) :-
	
    A is 2^3,
    assertion(A == 8).
    
:-end_tests(imprimirCurriculo).

