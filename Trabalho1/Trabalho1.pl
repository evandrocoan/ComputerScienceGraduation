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
quemMoraEm(Cidade, Nome) :- informacoesPessoais([Nome, _, Cidade, _]).

/* Questão 3 ###############################################################
 * Qual a idade de uma dada pessoa?
 * 
 * Primeiro, converto a data de aniversário para um TimeStamp. 
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

/* Pega a data de nascimento de uma dada pessoa e calcula sua idade 
 * Primeiro, encontra a pessoa na lista informacoesPessoais.
 * Segundo, pega sua DataDeNascimento da lista L de informações da Pessoa.
 * Terceiro, calcula sua idade.
 * */
qualIdadeDe(Nome, Idade) :- 
	informacoesPessoais(L), privado_DadoNaPosicao(NomeDaPessoa,[_|L],1),
	NomeDaPessoa = Nome,
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade).
	
/* Questão 4 ###############################################################
 * Quais as pessoas com mais de 30 anos?
 * 
 * Primeiro, carrego a Lista com os dados da pessoa.
 * Segundo, calculo a idade da pessoa.
 * Terceiro, calculo se a pessoa é maior que 30 anos.
 * Quarto, pego o nome da pessoa e retorno na variável Nome.
 * */
quaisComMaisDe30Anos(Nome) :-
	informacoesPessoais(L),
	privado_DadoNaPosicao(DataDeNascimento,[_|L],2),
	privado_CalcularIdade(DataDeNascimento, Idade),
	Idade > 30,
	privado_DadoNaPosicao(Nome,[_|L],1).

/* Questão 5 ###############################################################
 * Quais os cursos de uma dada pessoa?
 * 
 * Primeiro encontro a pessoa na lista.
 * Segundo pego o curso dela e retorna na variável Curso.
 * */
qualCursoDe(Nome, Curso) :-
	informacoesAcademicas(L), privado_DadoNaPosicao(DadoDeRetorno,[_|L],1),
	DadoDeRetorno = Nome,
	privado_DadoNaPosicao(Curso,[_|L],2).

/* Questão 6 ###############################################################
 * Quais os(as) orientadores(as) de uma dada pessoa?
 * 
 * Primeiro, pega a lista.
 * Segundo, pega o nome da pessoa e verifica se é a pessoa que queremos.
 * Terceiro, pega o nome do orientador.
 * */
quaisOrientadoresDe(Nome, Orientadores) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(NomeDaPessoa, [_|L], 1),
	NomeDaPessoa = Nome,
	privado_DadoNaPosicao(Orientadores, [_|L], 4).

/* Questão 7 ###############################################################
 * Quais os colegas de curso ou de trabalho de uma dada pessoa?
 * 
 * Primeiro, tiro a parte inicial da lista.
 * Segundo, imprimo o restante da lista, isto é, o nome das referências.
 * */
privado_DividirLista(L,0,[],L).
privado_DividirLista([X|Xs],N,[X|Ys],Zs) :- 
	N > 0, 
	N1 is N - 1, 
	privado_DividirLista(Xs,N1,Ys,Zs).

/* Verifica se uma pessoa é membro da cabeça da lista. Faz isso para garantir 
 * que não seja retornado verdadeiro caso uma das pessoas no final da lista
 * seja encontrada. 
 * */
privado_is_head_member(P, L) :- L = [P|_].

/* Informa quem são os colegas de uma dada pessoa. Primeiro carrega a lista
 * de informacoesAcademicas em Lista, depois verifica se ela pertence à lista. 
 * Caso sim, retorna os colegas na variável Colegas. 
 * */
quaisColegasDe(Nome, Colegas) :-
	informacoesAcademicas(Lista), 
	privado_is_head_member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas).

/* Questão 8 ###############################################################
 * Quais as pessoas sem nenhum colega citado como referencia?
 * 
 * Primeiro, carrega-se a lista em L, pega-se uma pessoa e tira-se a parte 
 * inicial da lista.
 * Segundo, verifica-se o restante da lista, isto é, o nome das referências, 
 * é uma lista vazia, ou seja, caso tenha comprimento 0.
 * */
quaisNaoTemRerefencia(Nomes) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(Nomes, [_|L], 1),
	quaisColegasDe(Nomes, Colegas),
	length(Colegas, Tamanho),
	Tamanho = 0.

/* Questão 9
 * Qual o numero de colegas de uma dada pessoa?
 * 
 * Primeiro, carrega-se a informacoesAcademicas em Lista, 
 * Segundo, verifica-se se essa é a lista dessa pessoa.
 * Terceiro, retira-se da lista as outras informações, deixando somente os 
 * colegas.
 * Quarto, coloca-se o comprimento da lista, isto é, o número de colegas de
 * uma dada pessoa em um dado curso.
 * */
qualNumeroDeColegas(Nome, Quantidade) :-
	informacoesAcademicas(Lista), 
	privado_is_head_member(Nome, Lista), 
	privado_DividirLista(Lista, 6, _, Colegas),
	length(Colegas, Quantidade).

/* Questão 10
 * Quantas pessoas estudaram em uma dada instituicao?
 * 
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

/* Acrescenta um contador que já deve estar inicializado. Este predicado 
 * acrescenta o contador, chamado contador, em uma unidade (1).
 * Ele retorna sempre 1 na variavel Quantidade, para cada uma das ocorrências 
 * da universidade na variável informacoesAcademicas.
 * */
privado_QuantosEstudaramNa(Instituicao, Quantidade) :-
	informacoesAcademicas(L), 
	privado_DadoNaPosicao(InstituicaoDaPessoa, [_|L], 3),
	InstituicaoDaPessoa = Instituicao,
	Quantidade is 1.








