﻿importarArquivosExternos:-[bibliotecaDeListas,bancoDeDados].
:-importarArquivosExternos.

/* ############################ Programa ###################################*/
/* Questão 1 ###############################################################
 * Qual o telefone de uma dada pessoa?
 * */
qualTelefone(Nome_Interno, Telefones_Interno) :-
	/* Faz todas as requisições ';' para privado_QualTelefoneDe, e pega o 
	 *   uma lista contendo todos os telefones de uma dada pessoa. */
	findall( Telefone, privado_QualTelefone(Nome_Interno, Telefone), 
														Telefones_Interno).
	
	/* Primeiro testo se a pessoa é a cabeça da lista. 
	 * Segundo pego o telefone dela nesta lista na quarta posição. */
	privado_QualTelefone(Nome, Telefone) :- 
		informacoesPessoais([Nome, _, _, Telefone]).


/* Questão 2 ###############################################################
 * Quais as pessoas de uma dada cidade?
 * */
quemMoraEm(Cidade, Nomes) :-
	findall(Nome, privado_QuemMoraEm(Cidade, Nome), Nomes).
	
	privado_QuemMoraEm(Cidade, Nome) :- informacoesPessoais(
		[Nome, _, Cidade, _]).


/* Questão 3 ###############################################################
 * Qual a idade de uma dada pessoa?
 *
 * Pega a data de nascimento de uma dada pessoa e calcula sua idade.
 * Primeiro, encontra a pessoa na lista informacoesPessoais.
 * Segundo, pega sua DataDeNascimento da lista L de informações da Pessoa.
 * Terceiro, calcula sua idade.
 * */
qualIdadeDe(Nome, Idade) :- 
	informacoesPessoais(L), dadoNaPosicao(NomeDaPessoa, L, 0),
	NomeDaPessoa == Nome,
	dadoNaPosicao(DataDeNascimento, L, 1),
	privado_CalcularIdade(DataDeNascimento, Idade).

	/* Primeiro, converto a data de aniversário Data para um TimeStamp 
	 *  DataDeAniver. 
	 * Segundo, consigo a data atual e armazeno em outro TimeStamp DataAtual.
	 * Terceiro, obtenho Ano do aniversário de DataDeAniver.
	 * Quarto, obtenho Ano atual de DataDeAniver.
	 * Quinto, calculo a idade da pessoa.
	 * */
	privado_CalcularIdade(Data, Idade) :-
	    date_time_stamp(Data, DataDeAniver),
	    get_time(DataAtual),
	    convert_time(DataDeAniver, AnoAniver, _, _,_,_,_,_),
	    convert_time(DataAtual, AnoAtual, _, _, _,_,_,_),
	    Idade is AnoAtual - AnoAniver.


/* Questão 4 ###############################################################
 * Quais as pessoas com mais de 30 anos?
 * */
quaisComMaisDe30Anos(Nomes) :-
	findall(Nomes, privado_QuaisComMaisDe30Anos(Nomes), Nomes).

	/* Primeiro, carrego a Lista com os dados da pessoa.
	 * Segundo, calculo a idade da pessoa.
	 * Terceiro, calculo se a pessoa é maior que 30 anos.
	 * Quarto, pego o nome da pessoa e retorno na variável Nome.
	 * */
	privado_QuaisComMaisDe30Anos(Nome) :-
		informacoesPessoais(L),
		dadoNaPosicao(DataDeNascimento, L, 1),
		privado_CalcularIdade(DataDeNascimento, Idade),
		Idade > 30,
		dadoNaPosicao(Nome, L, 0).


/* Questão 5 ###############################################################
 * Quais os cursos de uma dada pessoa?
 * */
quaisCursosDe(Nome, CursosDaPessoa) :-
	findall(Curso, privado_quaisCursosDe(Nome, Curso), CursosDaPessoa).

	/* Primeiro encontro a pessoa na lista.
	 * Segundo pego o curso dela e retorna na variável Curso.
	 * */
	privado_quaisCursosDe(Nome, Curso) :-
		informacoesAcademicas(L), dadoNaPosicao(DadoDeRetorno,[_|L],1),
		DadoDeRetorno == Nome,
		dadoNaPosicao(Curso, L, 1).


/* Questão 6 ###############################################################
 * Quais os(as) orientadores(as) de uma dada pessoa?
 * */
quaisOrientadoresDe(Nome, OrientadoresDaPessoa) :-
	findall(Orientadores, 
		privado_QuaisOrientadoresDe(Nome, Orientadores), OrientadoresDaPessoa).

	/* Primeiro, pega a lista.
	 * Segundo, pega o nome da pessoa e verifica se é a pessoa que queremos.
	 * Terceiro, pega o nome do orientador.
	 * */
	privado_QuaisOrientadoresDe(Nome, Orientadores) :-
		informacoesAcademicas(L), 
		dadoNaPosicao(NomeDaPessoa, L, 0),
		NomeDaPessoa == Nome,
		dadoNaPosicao(Orientadores, L, 3).


/* Questão 7 ###############################################################
 * Quais os colegas de curso ou de trabalho de uma dada pessoa?
 * */
quaisColegasDe(Nome, ColegasDaPessoaNivelada) :-
	findall(Colegas, privado_QuaisDeFaculdade(Nome, Colegas), ColegasDeFacul ),
	findall(Colegas, privado_QuaisDeTrabalho(Nome, Colegas), ColegasDeTraba ),
	merge(ColegasDeFacul, ColegasDeTraba, ColegasDaPessoa),
	nivelada( ColegasDaPessoa, ColegasDaPessoaNivelada).

	/* Primeiro carrega a lista de informacoesAcademicas em Lista, depois 
	 * verifica se a lista pertence ao Nome. Caso sim, retorna uma lista dos 
	 * colegas na variável Colegas. 
	 * */
	privado_QuaisDeFaculdade(Nome, Colegas) :-
		/* Primeiro, tiro a parte inicial da lista.
	 	 * Segundo, retorno o restante da lista, isto é, o nome dos colegas.
		 * */
		informacoesAcademicas(Lista), 
		ehMemboDaCabeca(Nome, Lista), 
		dividirLista(Lista, 6, _, Colegas).
	
		/* Primeiro, tiro a parte inicial da lista.
	 	 * Segundo, retorno o restante da lista, isto é, o nome dos colegas.
		 * */
		privado_QuaisDeTrabalho(Nome, Colegas) :-
			informacoesProfissionais(Lista), 
			ehMemboDaCabeca(Nome, Lista), 
			dividirLista(Lista, 5, _, Colegas).


/* Questão 8 ###############################################################
 * Quais as pessoas sem nenhum colega citado como referencia?
 * */
quaisNaoTemReferencia(Nomes) :-
    /* Faz todas as requisições ';' para a variavel informacoesAcademicas e
     * Recebe um lista Nomes contendo os nomes, das pessoas que não tem 
     * referências.
     * */
    findall(Nome, privado_QuaisNaoTemReferencia(Nome), Nomes).

	/* Primeiro, carrega-se a lista em L, pega-se uma pessoa e tira-se a parte 
	 * inicial da lista.
	 * Segundo, verifica-se o restante da lista, isto é, o nome das 
	 * referências, é uma lista vazia, ou seja, caso tenha comprimento 0.
	 * */
	privado_QuaisNaoTemReferencia(Nomes) :-
		informacoesAcademicas(L), 
		dadoNaPosicao(Nomes, L, 0),
		privado_QuaisColegasDe1(Nomes, Colegas),
		length(Colegas, Tamanho),
		Tamanho == 0.


/* Questão 9 #################################################################
 * Qual o numero de colegas de uma dada pessoa?
 * */
qualNumeroDeColegas(Nome, QuantidadeDeColegas) :-
    /* Faz todas as requisições ';' para a variavel informacoesAcademicas e
     * Recebe um lista Lista contendo o número de colegas, para cada 
     * instituição em que o aluno estudou.
     * */
    findall(Quantidade, privado_QualNumeroDeColegas(Nome, Quantidade), Lista),

    /* Faz a soma da lista Lista e armazena na variável 
     * QuatidadeDeColegas, que é retornada como resposta do programa.
     * */
    somaDosElementos( Lista, QuantidadeDeColegas).

	/* Primeiro, carrega-se a informacoesAcademicas em Lista, 
	 * Segundo, verifica-se se essa é a lista dessa pessoa.
	 * Terceiro, retira-se da lista as outras informações, deixando somente os 
	 * colegas.
	 * Quarto, retorna-se o comprimento da lista, isto é, o número de colegas 
	 * de uma dada pessoa em um dado curso, na variável Quantidade.
	 * */
	privado_QualNumeroDeColegas(Nome, Quantidade) :-
		informacoesAcademicas(Lista), 
		privado_is_head_member(Nome, Lista), 
		dividirLista(Lista, 6, _, Colegas),
		length(Colegas, Quantidade).


/* Questão 10 ################################################################
 * Quantas pessoas estudaram em uma dada instituicao?
 * 
 * Primeiro, encontra todas as ocorencias da univerdade para cada uma delas
 * cria uma lista de 1s.
 * Segundo, retorna a soma dessa lista na variável QuatidadeDePessoas.
 * */
quantosEstudaramNa(Lugar, QuatidadeDePessoas) :-
    /* Faz todas as requisições ';' para a variavel informacoesAcademicas e
     * Recebe um lista Lista contendo 1s, para cada vez que a Instituticao eh 
     * encontrada em informacoesAcademicas.
     * */
    findall(Quantidade, privado_QuantosEstudaramNa(Lugar, Quantidade), Lista),

    /* Faz a soma da lista de 1s em Lista e armazena na variável 
     * QuatidadeDePessoas, que é retornada como resposta do programa.
     * */
    somaDosElementos( Lista, QuatidadeDePessoas).

	/* Este predicado retorna sempre 1 na variavel Quantidade, para cada uma 
	 * das ocorrências da universidade na variável informacoesAcademicas.
	 * */
	privado_QuantosEstudaramNa(Instituicao, Quantidade) :-
		informacoesAcademicas(L), 
		dadoNaPosicao(InstituicaoDaPessoa, L, 2),
		InstituicaoDaPessoa == Instituicao,
		Quantidade is 1.


/* Questão 11 ################################################################
 * Quais as pessoas com mais de 5 anos de experiencia em um cargo qualquer?
 * */
quemComMaisDe5Anos(NomesDasPessoas) :-
	/* Faz todas as requisições ';' para a variavel informacoesProfissionais e
     * Recebe um lista Nomes contendo os nomes.
     * */
    findall(Nome, privado_QuemComMaisDe5Anos(Nome), Nomes),
    
    /* Remove os elementos duplicados da lista.
     * */
    list_to_set(Nomes, NomesDasPessoas).

	/* Primeiro carrega-se a informacoesProfissionais em Lista,
	 * Segundo, calcula-se o tempo de experiencia da pessoa.
	 * Terceiro, retorna o nome da pessoa caso o tempo seja maior que 5.
	 * */
	privado_QuemComMaisDe5Anos(Nome) :- 
		informacoesProfissionais(L),
		dadoNaPosicao(DataInicial, L, 3),
		dadoNaPosicao(DataFinal,L, 4),
		TempoDeExperiencia is DataFinal - DataInicial,
		TempoDeExperiencia > 5,
		dadoNaPosicao(Nome, L, 0).


/* Questão 12 ################################################################
 * Qual o tempo de estudo de uma dada pessoa em cada curso ?
 * 
 * Primeiro carrega-se as listas de Informações academicas.
 * Segundo verifica-se se o nome informado é cabeça da lista.
 * Terceiro carrega-se a data inicio do curso.
 * Quarto carrega-se a data de término do curso.
 * Quinto retorna-se a diferença entre as datas.
 * */
tempoDeEstudoEmCadaCurso(NomeDaPessoa, TempoDeEstudo) :-
	informacoesAcademicas(L),
	dadoNaPosicao(Nome, L, 0),
	Nome == NomeDaPessoa,
	dadoNaPosicao(DataInicial, L, 4),
	dadoNaPosicao(DataFinal, L, 5),
	TempoDeEstudo is DataFinal - DataInicial.


/* Questão 13 ###############################################################
 * Qual o tempo total de estudo de uma dada pessoa?
 *
 * Primeiro cria-se uma lista com todos os nomes.
 * Segundo carrega-se as Informações Acadêmicas.
 * Terceiro e Quarto comparam o nome da lista com a nome na cabeça.
 * Quinto e Sexto carregam os valores de início e término dos cursos.
 * Sexto calcula o tempo de 1 curso.
 * Sétimo faz a soma dos tempos dos cursos.
 * */

tempoTotalDeEstudo(Nome, TempoTotal) :-
	findall(Tempo, privado_TempoDeCada(Nome, Tempo), Lista),
	somaDosTempos(Lista, TempoTotal).

	privado_TempoDeCada(Nome2, Tempo) :-
		informacoesAcademicas(L),
		dadoNaPosicao(NomeDaPessoa, [_|L], 1),
		NomeDaPessoa = Nome2,
		dadoNaPosicao(DataInicial,[_|L],5),
		dadoNaPosicao(DataFinal,[_|L],6),
		Tempo is DataFinal - DataInicial.


/* Questão 14 #############################################################
 * Qual o tempo medio de estudo entre todas as pessoas?
 * 
 * Primeiro cria-se uma lista com o tempo de estudo de cada pessoa, em todos 
 * os cursos.
 * Segundo conta-se a quantidade de pessoas no banco de dados.
 * Terceiro carregam-se as listas de Informações Acadêmicas.
 * Quarto acessa todas as listas.
 * Quinto e Sexto carregam os valores de início e término dos cursos.
 * Sétimo calcula o tempo de 1 curso.
 * Oitavo incrementa a lista de pessoas em mais um.
 * Nono faz a soma dos tempos dos cursos. 
 * Décimo faz a soma da lista de pessoas.
 * Décimo Primeiro calculo o tempo médio.
 * */
tempoMedioDeEstudo(TempoMedio) :-
	findall(Tempo, privado_TempoDeCada2(_,Tempo), ListaTempos),
	findall(Retorno, privado_TempoDeCada2(Retorno,_), Listade1),
	somaDosTempos( ListaTempos, TempoTotal),
	somaDosTempos( Listade1, Retorno),
	TempoMedio is TempoTotal / Retorno.

	privado_TempoDeCada2(Retorno, Tempo) :-
		informacoesAcademicas(L),
		dadoNaPosicao(_,[_|L], 1),
		dadoNaPosicao(DataInicial,[_|L],5),
		dadoNaPosicao(DataFinal,[_|L],6),
		Tempo is DataFinal - DataInicial,
		Retorno is 1.


/* Questão 15 ################################################################
 * Qual o nome de todas as pessoas da lista ?
 * */
nomeDeTodasAsPessoas(ListaDeNomes) :-
	findall(Nome, privado_Nomes(Nome), ListaDeNomesTemp),
	sort(ListaDeNomesTemp, ListaDeNomes).

	privado_Nomes(Nome) :-
		dadoDeInfoNaPosicao(1,Nome).



