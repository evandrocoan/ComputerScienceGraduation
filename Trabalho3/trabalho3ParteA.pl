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


%##################################### limiarizacao #############################################
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
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
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
    write( NovaCoordenada_ColunaAtual ), write(','),
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


%##################################### negativo #################################################
/* Negativo: para cada intensidade I na imagem de entrada, produz-se 255 - I na imagem de saída 
 *   se a entrada for binária, a subtração passa a ser 1 - I.
 * 
 * 
 * */
negativo( Matriz, NovaMatriz ) :-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( matrix, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, 0 ), 
    
    privado_Negativo_ComputarMatriz( Matriz ),
    nb_getval( matrix, NovaMatriz ), nl, nl, 
    write( NovaMatriz ),
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_Negativo_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ), 
    nl, nl, 
    
    privado_Negativo_ComputarLinhas( LinhaAtual ), 
    fail.


    /* Faz a failure-driven loop 'privado_Negativo_ComputarMatriz' retornar true ao invés de 
     *   falhar.
     * */
    privado_Negativo_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_Negativo_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    write( Coordenada_LinhaAtual ), write(','),
    write( Coordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_Negativo_ComputarElementos( 
                               Coordenada_LinhaAtual, Coordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_Negativo_ComputarLinhas' retornar true ao invés de 
     *   falhar.
     * */
    privado_Negativo_ComputarLinhas( _ ).


/* Executa o altoritmo do negativo na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_Negativo_ComputarElementos( LinhaAtual, ColunaAtual, ElementoAtual ) :-
    
    nb_getval( ehUmaImagemBinaria, EhUmaImagemBinaria ), 
    
    ( EhUmaImagemBinaria =:= 1 ->
    
        privado_Negativo_AlterarElemento( LinhaAtual, ColunaAtual, 0 )
    ;
        privado_Negativo_AlterarElemento( LinhaAtual, ColunaAtual, 1 )
    ).


/* Dada as coordenadas 'X, Y' da Matriz, substitui o elemento atual pelo NovoElemento.
 * */
privado_Negativo_AlterarElemento( X, Y, NovoElemento ) :-
    
    nb_getval( matrix, Matriz ),
    dadoNaPosicao( LinhaAtual, Matriz, X ),
    substituidoDaPos( NovoElemento, Y, LinhaAtual, NovaLinhaAtual ), 
    substituidoDaPos( NovaLinhaAtual, X, Matriz, NovaMatriz ),
    nb_setval( matrix, NovaMatriz ).


%###################################### ehUmaImagemBinaria ######################################
/* Recebe uma Matriz e retorna o parâmetro Binaria em 1 se a matriz passada como argumento é uma 
 *   matriz binária, e retorna 0 caso a Matriz não seja binária.
 * */
ehUmaImagemBinaria( Matriz, Binaria ) :-
	    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( ehMatrixBooleana, 0 ),
    nb_setval( larguraDaMatriz_Temporaria, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz_Temporaria, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual_Temporaria, -1 ), 
    nb_setval( coordenada_ColunaAtual_Temporaria, 0 ), 
    
    privado_ehImagemBinaria_ComputarMatriz( Matriz ),
    nb_getval( ehMatrixBooleana, Booleano ),
    
    ( Booleano > 0 ->
    
        Binaria is 0
    ;
        Binaria is 1
    ),
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_ehImagemBinaria_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual_Temporaria, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual_Temporaria, NovaCoordenada_LinhaAtual ),
    
    privado_ehImagemBinaria_ComputarLinhas( LinhaAtual ),
    fail.


    /* Faz a failure-driven loop 'privado_ehImagemBinaria_ComputarMatriz' retornar true ao invés de 
     *   falhar.
     * */
    privado_ehImagemBinaria_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_ehImagemBinaria_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_ColunaAtual_Temporaria, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz_Temporaria, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual_Temporaria, NovaCoordenada_ColunaAtual ),

    ( ElementoAtual > 1 ->
    
        nb_setval( ehMatrixBooleana, 1 )
    ;
        true
    ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_ehImagemBinaria_ComputarLinhas' retornar true ao invés de 
     *   falhar.
     * */
    privado_ehImagemBinaria_ComputarLinhas( _ ).


%#################################################################################################




