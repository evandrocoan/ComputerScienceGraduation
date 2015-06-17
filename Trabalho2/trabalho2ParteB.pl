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
    privado_ConstruirGrafo_Recursao(Lista),
    !.

	/* Recebe uma Lista de todos os nomes que existem no Banco de Dados e adiciona todas as pessoas 
	 *   com suas referências ao Grafo.
	 * */
	privado_ConstruirGrafo_Recursao([]).
	    
	privado_ConstruirGrafo_Recursao(Lista) :-
	    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
	    primeiro(ElementoTemporario, PessoaAtual),
	    
	    % Adiciona as Referencias da pessoa PessoaAtual no grafo.
	    quaisColegasDe(PessoaAtual, Referencias), 
	    adicionarVertice(PessoaAtual), 
	    privado_AdicionarReferencias_Recursao(PessoaAtual, Referencias), 
	
	    privado_ConstruirGrafo_Recursao(RestoLista).

	/* Para uma dada Pessoa, adiciona todas as suas Referencias ao Grafo.
	 * */
	privado_AdicionarReferencias_Recursao(_, []).
	
	privado_AdicionarReferencias_Recursao(Pessoa, Lista) :-
	    dividirLista(Lista, 1, ElementoTemporario, RestoLista),
	    primeiro(ElementoTemporario, ReferenciaAtual),
	    
	    % Conecta a Pessoa a sua referência ReferenciaAtual no Grafo.
        adicionarVertice(ReferenciaAtual), 
	    conectar(Pessoa, ReferenciaAtual),
	
	    privado_AdicionarReferencias_Recursao(Pessoa, RestoLista).


/* Recebe um Vértice e adiciona ele ao Grafo 'grafo.pl'. Caso o vertice já 
 *   exista não faz nada.
 * */
adicionarVertice( Vertice ) :-
    
    ( existeVertice( Vertice ) -> 
    
        write('O vertice: '), write(Vertice), write(' ja existe!')
	;  
	    consult('Trabalho2/grafo.pl'), 
        write('Escrevendo o vertice: '), write( Vertice ), write(' no arquivo.'),
        Predicado =.. [vertice, Vertice ],
        assert(Predicado),
    
        /* Lista todas as clausulas e grava no arquivo */
        tell('Trabalho2/grafo.pl'), 
        listing(aresta),
        listing(vertice),
        told
    ).


/* Informa se os vértices estão conectados e torna este grafo não-orientado.
 * */
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice1, Vertice2), !.
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice2, Vertice1), !.


/* Verifica se um dado Vértice já existe no grafo.
 * */
existeVertice( Vertice ) :-
	
	% evita que o vértice seja inicializado
	nonvar(Vertice), 
	
	consult('Trabalho2/grafo.pl'), 
	vertice( Vertice ),
	!.


/* Verifica se um dado Aresta já existe no grafo e torna o grafo não orientado.
 * */
existeAresta( Vertice1, Vertice2 ) :-
    
    % evita que o vértice seja inicializado
    nonvar(Vertice1), 
    nonvar(Vertice2), 
    
    consult('Trabalho2/grafo.pl'), 
    aresta( Vertice1, Vertice2 ),
    !.

existeAresta( Vertice1, Vertice2 ) :-
    
    % evita que o vértice seja inicializado
    nonvar(Vertice1), 
    nonvar(Vertice2), 
    
    consult('Trabalho2/grafo.pl'), 
    aresta( Vertice2, Vertice1 ),
    !.


conectar(Vertice1, Vertice2):-
    
    ( not( existeAresta( Vertice1, Vertice2 ) ) ->
    
	    ( existeVertice( Vertice1 ) -> 
	    
	        ( existeVertice( Vertice2 ) -> 
		    
		        consult('Trabalho2/grafo.pl'), 
	            write('Conectando os vertices: '), write( Vertice1 ), write(', '), 
	            write( Vertice2 ), write(' no arquivo.'),
	            Predicado =.. [aresta, Vertice1, Vertice2 ], 
	            assert(Predicado),
	        
	            /* Lista todas as clausulas e grava no arquivo */
	            tell('Trabalho2/grafo.pl'), 
	            listing(aresta), 
	            listing(vertice),
	            told
		    ;  
	            write('O vertice2 nao existe!')
		    ) 
	    ;  
	        write('O vertice1 nao existe!')
	    )
    ;
        write('A aresta: '), write( aresta( Vertice1, Vertice2 ) ), write(' ja existe!')
    ).














