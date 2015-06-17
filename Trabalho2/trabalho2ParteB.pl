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
        write('Escrevendo o vertice: '), write( Vertice ), write(' no arquivo.'), nl, 
        assert( vertice(Vertice) )
    ).


/* Informa se os vértices carregados em memória estão conectados e torna o grafo não-orientado.
 * */
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice1, Vertice2), !.
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice2, Vertice1), !.


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


conectar(Vertice1, Vertice2):-
    
    ( not( existeAresta( Vertice1, Vertice2 ) ) ->
    
	    ( existeVertice( Vertice1 ) -> 
	    
	        ( existeVertice( Vertice2 ) -> 
		    
	            write('Conectando os vertices: '), write( Vertice1 ), write(', '), 
	            write( Vertice2 ), write(' no arquivo.'), nl, 
	            assert( aresta(Vertice1, Vertice2) )
		    ;  
	            write('O vertice2 nao existe!'), nl
		    ) 
	    ;  
	        write('O vertice1 nao existe!'), nl
	    )
    ;
        write('A aresta: '), write( aresta( Vertice1, Vertice2 ) ), write(' ja existe!'), nl
    ).


/* Lista todas as clausulas e grava o grafo no arquivo 'Trabalho2/grafo.pl' e limpa a memória.
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
    retractall( aresta(_,_) ),
    retractall( vertice(_) ).


/* Carrega o grafo salvo no arquivo 'Trabalho2/grafo.pl' em memória.
 * */
carregarGrafo :-
	consult('Trabalho2/grafo.pl').








