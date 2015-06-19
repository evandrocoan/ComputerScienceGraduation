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
importarArquivoImagem :- [ 'Trabalho3/imagem.pl' ].
:-importarArquivoImagem.

importarTrabalho2ParteB :- [ 'Trabalho2/trabalho2ParteB.pl' ].
:-importarTrabalho2ParteB.


/* Limiarização (thresholding): dado um valor T como argumento, para cada intensidade I < T na 
 *   imagem de entrada, o pixel correspondente na imagem resultante se torna zero; para I > T, 
 *   a saída se torna um (produz-se uma imagem binária).
 *
 * Recebe uma IntensidadeT, e uma Matriz como parâmetro, e retorna uma NovaMatriz contendo a 
 *   limiarização.
 * Primeiro, calcula as dimensões da matriz, inicializa contadores que marcam a posição atual do 
 *   elemento que se está processando na matriz e salva tudo como variáveis globais.
 * */
limiarizacao( IntensidadeT, Matriz, NovaMatriz ) :-
	
	dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
	length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( matrix, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( intensidadeT, IntensidadeT), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, 0 ), 
    
    privado_Limiarizacao_ComputarMatriz( Matriz ),
    nb_getval( matrix, NovaMatriz ), nl, nl, 
    write( NovaMatriz ),
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_Limiarizacao_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ),
    nl, nl, 
    
    privado_Limiarizacao_ComputarLinhas( LinhaAtual ),
    fail.


    /* Faz a failure-driven loop 'privado_Limiarizacao_ComputarMatriz' retornar true ao invés de 
     *   falhar.
     * */
    privado_Limiarizacao_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_Limiarizacao_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    write( Coordenada_LinhaAtual ), write(','),
    write( Coordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_Limiarizacao_ComputarElementos( 
                               Coordenada_LinhaAtual, Coordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_Limiarizacao_ComputarLinhas' retornar true ao invés de 
     *   falhar.
     * */
    privado_Limiarizacao_ComputarLinhas( _ ).


/* Executa o altoritmo de limiarizacao na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_Limiarizacao_ComputarElementos( LinhaAtual, ColunaAtual, ElementoAtual ) :-
	
    nb_getval( intensidadeT, IntensidadeT ), 
	
	( ElementoAtual < IntensidadeT ->
	
        privado_Limiarizacao_AlterarElemento( LinhaAtual, ColunaAtual, 0 )
	;
        privado_Limiarizacao_AlterarElemento( LinhaAtual, ColunaAtual, 1 )
	).


/* Dada as coordenadas 'X, Y' da Matriz, substitui o elemento atual pelo NovoElemento.
 * */
privado_Limiarizacao_AlterarElemento( X, Y, NovoElemento ) :-
	
    nb_getval( matrix, Matriz ),
    dadoNaPosicao( LinhaAtual, Matriz, X ),
    substituidoDaPos( NovoElemento, Y, LinhaAtual, NovaLinhaAtual ), 
    substituidoDaPos( NovaLinhaAtual, X, Matriz, NovaMatriz ),
    nb_setval( matrix, NovaMatriz ).














