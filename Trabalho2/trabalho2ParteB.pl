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


/* Escreva uma regra para a criação apropriada de um grafo (escolha uma das 
 *   formas vistas em aula) de todas as pessoas (nós ou vértices) e suas 
 *   respectivas referências (arestas). 
 * */
%construirGrafo(Grafo) :-
	
	/* Cria um grafo com as referências de uma pessoa.
	 * 
	construirGrafo_Privado(Nome, Grafo):-
		quaisColegasDe(Nome, Referencias),*/


/* Recebe um Vértice e adiciona ele ao Grafo 'grafo.pl'. Caso o vertice já 
 *   exista não faz nada.
 * */
adicionarVertice( Vertice ) :-
    consult('Trabalho2/grafo.pl'), 
    ( existeVertice( Vertice ) -> 
    
        write('O vertice: '), write(Vertice), write(' ja existe!')
	;  
        write('Escrevendo o vertice: '), write( Vertice ), write(' no arquivo.'),
        Predicado =.. [vertice, Vertice ],
        assert(Predicado),
    
        /* Lista todas as clausulas e grava no arquivo */
        tell('Trabalho2/grafo.pl'), 
        listing(aresta),
        listing(vertice),
        told
    ).


/* Informa se os vértices estão conectados e torna o grafo não-orientado.
 * */
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice1, Vertice2).
estaoConectados(Vertice1, Vertice2) :- aresta(Vertice2, Vertice1).


/* Verifica se um dado Vértice já existe no grafo.
 * */
existeVertice( Vertice ) :-
    consult('Trabalho2/grafo.pl'), 
    vertice( Vertice ).


conectar(Vertice1, Vertice2):-
    consult('Trabalho2/grafo.pl'), 
    ( existeVertice( Vertice1 ) -> 
    
        ( existeVertice( Vertice2 ) -> 
	    
            write('Conectando os vertices: '), write( Vertice1 ), write(', '), write( Vertice2 ), 
            write(' no arquivo.'),
            Predicado =.. [aresta, Vertice1, Vertice2 ],
            assert(Predicado),
        
            /* Lista todas as clausulas e grava no arquivo */
            tell('Trabalho2/grafo.pl'), 
            listing(aresta),
            listing(vertice),
            told
	    ;  
            write('O vertice: '), write(Vertice2), write(' nao existe!')
	    ) 
    ;  
        write('O vertice: '), write(Vertice1), write(' nao existe!')
    ).
















