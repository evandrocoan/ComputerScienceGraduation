/* Para funcionar, dever selecionar o working directory para uma pasta acima
 *   da pasta Trabalho2 executando o arquivo de configuração.
 *
 * Muda para o diretório de trabalho padrão.
 *
 * irParaDiretorioPadrao :- 
 *  
 *  working_directory(_, 'D:/Evandro/Archives/Dropbox/Aplicativos/
 *                     SoftwareVersioning/2015-1_ParadigmasDeProgramacao').   
 *
 * :-irParaDiretorioPadrao.
 * */
importarTrabalho2ParteA :- ['Trabalho2/trabalho2ParteA.pl'].
:-importarTrabalho2ParteA.


/* Escreva uma regra para a criação apropriada de um grafo (escolha uma das formas vistas em 
 *   aula) de todas as pessoas (nós ou vértices) e suas respectivas referências (arestas). E 
 *   faça a persistência deste grafo em um arquivo '.pl'.
 * 
 * Realiza a construção do grafo. Primeiro, cria uma lista contendo o nome de todas as pessoas que 
 *   possuem um currículo no banco de dados utilizado o predicado criado anteriormente na parte A 
 *   do trabalho.
 * Segundo, carrega o grafo que está salvo no arquivo, caso já haja algum grafo definido 
 *   anteriormente.
 * */
construirGrafo :-
	
    privado_CarregaListaNomes(Lista),
    carregarGrafo,
    privado_ConstruirGrafo_Recursao(Lista),
    gravarGrafo,
    !.

	/* Recebe uma Lista de todos os nomes que existem no Banco de Dados e adiciona todas as pessoas 
	 *   com suas referências para a memória de predicados.
	 * */
	privado_ConstruirGrafo_Recursao([]).
	    
	privado_ConstruirGrafo_Recursao(Lista) :-
		
	    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
	    primeiro(ElementoTemporario, PessoaAtual),
	    
	    % Adiciona as Referencias da pessoa PessoaAtual para a memória.
	    quaisColegasDe(PessoaAtual, Referencias), nl,
	    adicionarVertice(PessoaAtual), 
	    privado_AdicionarReferencias_Recursao(PessoaAtual, Referencias), 
        
	    privado_ConstruirGrafo_Recursao(RestoLista).

	/* Para uma dada Pessoa, adiciona todas as suas Referencias para a memória.
	 * */
	privado_AdicionarReferencias_Recursao(_, []) :- !.
	
	privado_AdicionarReferencias_Recursao(Pessoa, Lista) :-
		
	    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
	    primeiro(ElementoTemporario, ReferenciaAtual),
	    
	    % Conecta a Pessoa a sua referência ReferenciaAtual na carregados na memória.
        adicionarVertice(ReferenciaAtual), 
	    conectar(Pessoa, ReferenciaAtual),
	   
	    privado_AdicionarReferencias_Recursao(Pessoa, RestoLista).


/* Recebe um Vértice e adiciona ele para a memória. Caso o vertice já exista, não se faz nada.
 * */
adicionarVertice( Vertice ) :-
    
    ( existeVertice( Vertice ) ->  
    
        write('O vertice: '), write(Vertice), write(' ja existe!'), nl
	;  
        write('Adicionando o vertice: '), write( Vertice ), write('.'), nl, 
        assert( vertice(Vertice) )
    ).


/* Informa se os vértices carregados em memória estão conectados.
 * */
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice1, Vertice2).


/* Verifica se um dado Vértice já existe carregado em memória.
 * */
existeVertice( Vertice ) :-
	
	% evita que o vértice seja inicializado
	nonvar(Vertice), 

	vertice( Vertice ),
	!.


/* Verifica se um dado Aresta já existe em memória e torna o grafo não orientado.
 * */
existeAresta( Vertice1, Vertice2 ) :-
    
    % evita que o vértice seja inicializado
    nonvar(Vertice1), 
    nonvar(Vertice2), 
    
    aresta( Vertice1, Vertice2 ),
    !.

	existeAresta( Vertice1, Vertice2 ) :-
	    
	    % evita que o vértice seja inicializado
	    nonvar(Vertice1), 
	    nonvar(Vertice2), 
	    
	    aresta( Vertice2, Vertice1 ),
	    !.


/* Conecta o Vertice1 com o Vertice2 no grafo. 
 * */
conectar(Vertice1, Vertice2):-
    
    ( not( existeAresta( Vertice1, Vertice2 ) ) ->
    
	    ( existeVertice( Vertice1 ) -> 
	    
	        ( existeVertice( Vertice2 ) -> 
		    
	            write('Conectando os vertices: '), write( Vertice1 ), write(', '), 
	            write( Vertice2 ), write('.'), nl, 
	            assert( aresta(Vertice1, Vertice2) ),
	            assert( aresta(Vertice2, Vertice1) )
		    ;  
	            write('O vertice2 nao existe!'), nl
		    ) 
	    ;  
	        write('O vertice1 nao existe!'), nl
	    )
    ;
        write('A aresta: '), write( aresta( Vertice1, Vertice2 ) ), write(' ja existe!'), nl
    ).


/* Lista todas as claúsulas e grava o grafo no arquivo 'Trabalho2/grafo.pl' e limpa a memória.
 * */
gravarGrafo :-
	    
    tell('Trabalho2/grafo.pl'), 
    listing(aresta), 
    listing(vertice),
    told,
    limparMemoria.


/* Limpa a memória removendo o grafo dela.
 * */
limparMemoria :-
    abolish( aresta/2 ),
    retractall( vertice(_) ).


/* Carrega o grafo salvo no arquivo 'Trabalho2/grafo.pl' em memória.
 * */
carregarGrafo :-
	consult('Trabalho2/grafo.pl').


/* Dadas duas pessoas P1 e P2, escreva uma regra que devolva uma lista de um conjunto de outras 
 *   pessoas (uma ligada a outra) que possam conectá-las. Ou seja, apresente caminhos possíveis 
 *   no grafo de P1 a P2 (pode não haver caminho possível ou as duas pessoas serem ligadas 
 *   diretamente).
 * 
 * Primeiro carrega o grafo em memória. Segundo calcula a lista de caminhos utilizando findall.
 * */
conexoes(P1, P2, Lista) :-
    findall( ListaSaida, privado_viajarPeloGrafo( P1, P2, ListaSaida ), Lista ).

    privado_viajarPeloGrafo( P1, P2, ListaSaida ) :-
	    viajarPeloGrafo( P1, P2, [P1], Q ),
	    reverse( Q, ListaSaida ).

	viajarPeloGrafo( P1, P2, P, [P2|P] ) :-
	    estaoConectados(P1,P2).

	viajarPeloGrafo( P1, P2, Visitado, Lista ) :-
	   estaoConectados( P1, C ),
	   C \== P2,
	   \+member( C, Visitado ),
	   viajarPeloGrafo( C, P2, [C|Visitado], Lista).


/* Mostre o menor número de passos em um caminho entre P1 e P2. Por exemplo: preciso de passos 
 *   para chegar a 'Don Stephens'.
 * 
 * Primeiro, pega a lista de caminhos consultando 'conexoes(P1, P2, Lista)'.
 * Segundo, dentre a lista de listas, determima qual a menor e retorna o valor do menor número de
 *   passos em Passos.
 * */
menorCaminho(P1, P2, Passos) :-
	
	conexoes(P1, P2, Lista),
	privado_Caminho_Recursao(Lista, Resultado),
	menorElemento(Resultado, Passos),
	!.
	

/* Passa em todos os elementos da Lista de listas e retorna uma lista contendo os tamanhos de 
 *   cada uma das listas que a Lista contém.
 * */
privado_Caminho_Recursao(Lista, Resultado) :- privado_Caminho_RecursaoInterno(Lista, [], Resultado).

privado_Caminho_RecursaoInterno([], Temporario, Resultado) :- copy_term(Temporario, Resultado).

privado_Caminho_RecursaoInterno(Lista, AcumuladorEntrada, Resultado) :-
	
    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
    primeiro(ElementoTemporario, ListaAtual),
    
    % Fazer as operações sobre o ElementoAtual da lista.
    length(ListaAtual, Tamanho),
    TamanhoReal is Tamanho - 1,
    inseridoNoFinal( TamanhoReal, AcumuladorEntrada, AcumuladorSaida ),

    privado_Caminho_RecursaoInterno(RestoLista, AcumuladorSaida, Resultado).









