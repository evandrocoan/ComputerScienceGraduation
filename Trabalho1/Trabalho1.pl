importarArquivosExternos:-[biblioteca,bancoDeDados].
:-importarArquivosExternos.

/* ############################ Programa ###################################*/
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
quemMoraEm(Cidade, Nomes) :-
	findall(Nome, privado_QuemMoraEm(Cidade, Nome), Nomes).
privado_QuemMoraEm(Cidade, Nome) :- informacoesPessoais([Nome, _, Cidade, _]).

/* Questão 3 ###############################################################
 * Qual a idade de uma dada pessoa?
 *
 * Pega a data de nascimento de uma dada pessoa e calcula sua idade.
 * Primeiro, encontra a pessoa na lista informacoesPessoais.
 * Segundo, pega sua DataDeNascimento da lista L de informações da Pessoa.
 * Terceiro, calcula sua idade.
 * */
qualIdadeDe(Nome, Idade) :- 
	informacoesPessoais(L), privado_DadoNaPosicao(NomeDaPessoa,[_|L],1),
	NomeDaPessoa == Nome,
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade).

/* Primeiro, converto a data de aniversário para um TimeStamp. 
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
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade),
	Idade > 30,
	privado_DadoNaPosicao(Nome,[_|L],1).

/* Questão 5 ###############################################################
 * Quais os cursos de uma dada pessoa?
 * */
qualCursoDe(Nome, CursosDaPessoa) :-
	findall(Curso, privado_QualCursoDe(Nome, Curso), CursosDaPessoa).

/* Primeiro encontro a pessoa na lista.
 * Segundo pego o curso dela e retorna na variável Curso.
 * */
privado_QualCursoDe(Nome, Curso) :-
	informacoesAcademicas(L), privado_DadoNaPosicao(DadoDeRetorno,[_|L],1),
	DadoDeRetorno == Nome,
	privado_DadoNaPosicao(Curso,[_|L],2).

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
	privado_DadoNaPosicao(NomeDaPessoa, [_|L], 1),
	NomeDaPessoa == Nome,
	privado_DadoNaPosicao(Orientadores, [_|L], 4).

/* Questão 7 ###############################################################
 * Quais os colegas de curso ou de trabalho de uma dada pessoa?
 * */
quaisColegasDe(Nome, ColegasDaPessoa) :-
	findall(Colegas, privado_QuaisColegasDe1(Nome, Colegas), ColegasDaPessoa1 ),
	findall(Colegas, privado_QuaisColegasDe2(Nome, Colegas), ColegasDaPessoa2 ),
	merge(ColegasDaPessoa1, ColegasDaPessoa2, ColegasDaPessoa).

/* Primeiro carrega a lista de informacoesAcademicas em Lista, depois verifica 
 * se a lista pertence ao Nome. Caso sim, retorna uma lista dos colegas na 
 * variável Colegas. 
 * */
privado_QuaisColegasDe1(Nome, Colegas) :-
	/* Primeiro, tiro a parte inicial da lista.
 	 * Segundo, retorno o restante da lista, isto é, o nome dos colegas.
	 * */
	informacoesAcademicas(Lista), 
	privado_Is_Head_Member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas).

privado_QuaisColegasDe2(Nome, Colegas) :-
	/* Primeiro, tiro a parte inicial da lista.
 	 * Segundo, retorno o restante da lista, isto é, o nome dos colegas.
	 * */
	informacoesProfissionais(Lista), 
	privado_Is_Head_Member(Nome, Lista), 
	privado_DividirLista(Lista, 5, _, Colegas).

/* Questão 8 ###############################################################
 * Quais as pessoas sem nenhum colega citado como referencia?
 * */
quaisNaoTemRerefencia(Nomes) :-
    /* Faz todas as requisições ';' para a variavel informacoesAcademicas e
     * Recebe um lista Nomes contendo os nomes, das pessoas que não tem 
     * referências.
     * */
    findall(Nome, privado_QuaisNaoTemRerefencia(Nome), Nomes).

/* Primeiro, carrega-se a lista em L, pega-se uma pessoa e tira-se a parte 
 * inicial da lista.
 * Segundo, verifica-se o restante da lista, isto é, o nome das referências, 
 * é uma lista vazia, ou seja, caso tenha comprimento 0.
 * */
privado_QuaisNaoTemRerefencia(Nomes) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(Nomes, [_|L], 1),
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
 * Quarto, retorna-se o comprimento da lista, isto é, o número de colegas de
 * uma dada pessoa em um dado curso, na variável Quantidade.
 * */
privado_QualNumeroDeColegas(Nome, Quantidade) :-
	informacoesAcademicas(Lista), 
	privado_is_head_member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas),
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
	privado_DadoNaPosicao(InstituicaoDaPessoa, [_|L], 3),
	InstituicaoDaPessoa == Instituicao,
	Quantidade is 1.

/* Questão 11
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
	privado_DadoNaPosicao(DataInicial,[_|L],4),
	privado_DadoNaPosicao(DataFinal,[_|L],5),
	TempoDeExperiencia is DataFinal - DataInicial,
	TempoDeExperiencia > 5,
	privado_DadoNaPosicao(Nome,[_|L],1).







