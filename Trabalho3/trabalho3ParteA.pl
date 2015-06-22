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
    write( NovaMatriz ), nl, nl,
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
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_Limiarizacao_ComputarElementos( 
                               Coordenada_LinhaAtual, NovaCoordenada_ColunaAtual, ElementoAtual ),
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
 * Recebe uma Matriz como parâmetro, e retorna uma NovaMatriz contendo a matriz de imagem negativa.
 * Primeiro, calcula se a matriz da imagem é binária e as dimensões da matriz, inicializa 
 *   contadores que marcam a posição atual do elemento que se está processando na matriz e salva 
 *   tudo como variáveis globais.
 * */
negativo( Matriz, NovaMatriz ) :-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    ehUmaImagemBinaria( Matriz, Binaria ),
    
    nb_setval( ehUmaImagemBinaria, Binaria ),
    nb_setval( matrix, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
    privado_Negativo_ComputarMatriz( Matriz ),
    nb_getval( matrix, NovaMatriz ), nl, nl, 
    write( NovaMatriz ), nl, nl,
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
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_Negativo_ComputarElementos( 
                               Coordenada_LinhaAtual, NovaCoordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_Negativo_ComputarLinhas' retornar true ao invés de 
     *   falhar.
     * */
    privado_Negativo_ComputarLinhas( _ ).


/* Executa o algoritmo do negativo na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_Negativo_ComputarElementos( LinhaAtual, ColunaAtual, ElementoAtual ) :-
    
    nb_getval( ehUmaImagemBinaria, EhUmaImagemBinaria ), 
    
    ( EhUmaImagemBinaria =:= 1 ->
        
        NovoElemento is 1 - ElementoAtual
    ;
        NovoElemento is 255 - ElementoAtual
    ),
    privado_Negativo_AlterarElemento( LinhaAtual, ColunaAtual, NovoElemento ).


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
    nb_setval( coordenada_ColunaAtual_Temporaria, -1 ), 
    
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


    /* Faz a failure-driven loop 'privado_ehImagemBinaria_ComputarMatriz' retornar true ao invés  
     *   de falhar.
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
    
    
    /* Faz a failure-driven loop 'privado_ehImagemBinaria_ComputarLinhas' retornar true ao invés  
     *   de falhar.
     * */
    privado_ehImagemBinaria_ComputarLinhas( _ ).


%###################################### somaDeConstante ########################################
/* Soma de constante: dado um valor k, para cada intensidade I na imagem de entrada, produz-se 
 *   I + k na imagem resultante; no entanto, se (I + k) > 255, o valor da soma deve se tornar 
 *   255; k < 0 e ( I + k) < 0, então o valor da soma deve se tornar 0.
 * 
 * Dada uma contante K e uma Matriz, retorna uma NovaMatriz contendo a constante adicionada.
 * */
somaDeConstante( K, Matriz, NovaMatriz ) :-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( k, K ),
    nb_setval( matrix, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
    privado_somaDeConstante_ComputarMatriz( Matriz ),
    nb_getval( matrix, NovaMatriz ), nl, nl, 
    write( NovaMatriz ), nl, nl,
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_somaDeConstante_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ), 
    nl, nl, 
    
    privado_somaDeConstante_ComputarLinhas( LinhaAtual ), 
    fail.


    /* Faz a failure-driven loop 'privado_somaDeConstante_ComputarMatriz' retornar true ao invés  
     *   de falhar.
     * */
    privado_somaDeConstante_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_somaDeConstante_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_somaDeConstante_ComputarElementos( 
                               Coordenada_LinhaAtual, NovaCoordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_somaDeConstante_ComputarLinhas' retornar true ao invés  
     *   de falhar.
     * */
    privado_somaDeConstante_ComputarLinhas( _ ).


/* Executa o algoritmo do negativo na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_somaDeConstante_ComputarElementos( LinhaAtual, ColunaAtual, ElementoAtual ) :-
    
    nb_getval( k, K ),
    
    Atual is ElementoAtual + K,
    nb_setval( novo_Elemento, Atual ), 
        
    ( Atual < 0 ->
        
        AtualCorrigido is 0,
        nb_setval( novo_Elemento , AtualCorrigido)
    ;
        true
    ), 
    ( Atual >= 256 ->

        AtualCorrigido is 255, 
        nb_setval( novo_Elemento , AtualCorrigido)
    ;
        true
    ),
    privado_somaDeConstante_AlterarElemento( LinhaAtual, ColunaAtual ).


/* Dada as coordenadas 'X, Y' da Matriz, substitui o elemento atual pelo NovoElemento.
 * */
privado_somaDeConstante_AlterarElemento( X, Y ) :-
    
    nb_getval( novo_Elemento, Novo_Elemento ),
    nb_getval( matrix, Matriz ),
    dadoNaPosicao( LinhaAtual, Matriz, X ),
    substituidoDaPos( Novo_Elemento, Y, LinhaAtual, NovaLinhaAtual ), 
    substituidoDaPos( NovaLinhaAtual, X, Matriz, NovaMatriz ),
    nb_setval( matrix, NovaMatriz ).


%###################################### somaEntreImagens ########################################
/* Soma de constante: dado um valor k, para cada intensidade I na imagem de entrada, produz-se 
 *   I + k na imagem resultante; no entanto, se (I + k) > 255, o valor da soma deve se tornar 
 *   255; k < 0 e ( I + k) < 0, então o valor da soma deve se tornar 0.
 * 
 * Dada uma Matriz e uma OutraMatriz, retorna uma NovaMatriz contendo a soma das matrizes.
 * */
somaEntreImagens( Matriz, OutraMatriz, NovaMatriz ) :-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( outraMatriz, OutraMatriz ),
    nb_setval( matrix, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
    privado_somaEntreImagens_ComputarMatriz( Matriz ),
    nb_getval( matrix, NovaMatriz ), nl, nl, 
    write( NovaMatriz ), nl, nl,
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_somaEntreImagens_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ), 
    nl, nl, 
    
    privado_somaEntreImagens_ComputarLinhas( LinhaAtual ), 
    fail.


    /* Faz a failure-driven loop 'privado_somaEntreImagens_ComputarMatriz' retornar true ao invés  
     *   de falhar.
     * */
    privado_somaEntreImagens_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_somaEntreImagens_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_somaEntreImagens_ComputarElementos( 
                               Coordenada_LinhaAtual, NovaCoordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_somaEntreImagens_ComputarLinhas' retornar true ao invés  
     *   de falhar.
     * */
    privado_somaEntreImagens_ComputarLinhas( _ ).


/* Executa o algoritmo de somaEntreImagens na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_somaEntreImagens_ComputarElementos( LinhaAtual, ColunaAtual, ElementoAtual ) :-
    
    nb_getval( outraMatriz, OutraMatriz ),
    privado_somaEntreImagens_ObterElemento( 
                                      ColunaAtual, LinhaAtual, OutraMatriz, OutroElementoAtual ), 
    Atual is ElementoAtual + OutroElementoAtual, 
    nb_setval( novo_Elemento, Atual ), 

    ( Atual < 0 ->
        
        AtualCorrigido is 0,
        nb_setval( novo_Elemento , AtualCorrigido)
    ;
        true
    ), 
    ( Atual >= 256 ->

        AtualCorrigido is 255, 
        nb_setval( novo_Elemento , AtualCorrigido)
    ;
        true
    ),
    privado_somaEntreImagens_AlterarElemento( LinhaAtual, ColunaAtual ).


/* Dada as coordenadas 'X, Y' da Matriz, substitui o elemento atual pelo NovoElemento.
 * */
privado_somaEntreImagens_AlterarElemento( X, Y ) :-
    
    nb_getval( novo_Elemento, Novo_Elemento ), 
    nb_getval( matrix, Matriz ), 
    dadoNaPosicao( LinhaAtual, Matriz, X ), 
    substituidoDaPos( Novo_Elemento, Y, LinhaAtual, NovaLinhaAtual ), 
    substituidoDaPos( NovaLinhaAtual, X, Matriz, NovaMatriz ), 
    nb_setval( matrix, NovaMatriz ).


/* Dada as coordenadas 'X, Y' da OutraMatriz, retorna o elemento ElementoObtido que se encontra 
 *   nesta posição.
 * */
privado_somaEntreImagens_ObterElemento( X, Y, OutraMatriz, ElementoObtido ) :-
    
    dadoNaPosicao( LinhaAtual, OutraMatriz, Y ), 
    dadoNaPosicao( ElementoObtido, LinhaAtual, X ).


%###################################### pixelsIsolados ########################################
/* Detecção de pixels isolados: um pixel de intensidade I é isolado se seus quatro vizinhos 
 *   (de de baixo, da esquerda e da direita) têm intensidades menores que I.
 * 
 * Dada uma Matriz, retorna uma Lista de listas de três elementos contendo a posição e valor 
 *   do pixel isolado.
 * */
pixelsIsolados( Matriz, Lista ) :-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( lista, [] ),
    nb_setval( matriz, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
    privado_pixelsIsolados_ComputarMatriz( Matriz ),
    nb_getval( lista, Lista ), nl, nl, 
    write( Lista ), nl, nl,
    !.


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_pixelsIsolados_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ), 
    nl, nl, 
    
    privado_pixelsIsolados_ComputarLinhas( LinhaAtual ), 
    fail.


    /* Faz a failure-driven loop 'privado_pixelsIsolados_ComputarMatriz' retornar true ao invés de 
     *   falhar.
     * */
    privado_pixelsIsolados_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_pixelsIsolados_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_pixelsIsolados_ComputarElementos( 
                               Coordenada_LinhaAtual, NovaCoordenada_ColunaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_pixelsIsolados_ComputarLinhas' retornar true ao invés de 
     *   falhar.
     * */
    privado_pixelsIsolados_ComputarLinhas( _ ).


/* Executa o algoritmo de pixelsIsolados na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_pixelsIsolados_ComputarElementos( Linha, Coluna, ElementoAtual ) :-
    
    nb_getval( matriz, Matriz ),

    nb_setval( isoladoEsquerda, 0 ),
    nb_setval( isoladoDireita, 0 ), 
    nb_setval( isoladoAcima, 0 ), 
    nb_setval( isoladoAbaixo, 0 ), 
    
    privado_pixelsIsolados_ObterElementoEsquerda( Coluna, Linha, Matriz, Esquerda ), 
    
    ( nonvar( Esquerda ) -> 
        
        ( ElementoAtual > Esquerda ->
            
            nb_setval( isoladoEsquerda, 1 )
        ;
            true
        )
    ;
        nb_setval( isoladoEsquerda, 1 )
    ),
    privado_pixelsIsolados_ObterElementoDireita( Coluna, Linha, Matriz, Direita ),
    
    ( nonvar( Direita ) -> 
        
        ( ElementoAtual > Direita ->
            
            nb_setval( isoladoDireita, 1 )
        ;
            true
        )
    ;
        nb_setval( isoladoDireita, 1 )
    ),
    privado_pixelsIsolados_ObterElementoAcima( Coluna, Linha, Matriz, Acima ),
    
	( nonvar( Acima ) -> 
	   
        ( ElementoAtual > Acima ->
            
            nb_setval( isoladoAcima, 1 )
        ;
            true
        )
	;
        nb_setval( isoladoAcima, 1 )
	),
	privado_pixelsIsolados_ObterElementoAbaixo( Coluna, Linha, Matriz, Abaixo ), 
    
    ( nonvar( Abaixo ) -> 

        ( ElementoAtual > Abaixo ->
            
            nb_setval( isoladoAbaixo, 1 )
        ;
            true
        )
    ;
        nb_setval( isoladoAbaixo, 1 )
    ),
    nb_getval( isoladoEsquerda, IsoladoEsquerda ),
    nb_getval( isoladoDireita, IsoladoDireita ), 
    nb_getval( isoladoAcima, IsoladoAcima ), 
    nb_getval( isoladoAbaixo, IsoladoAbaixo ), 
    
    ( IsoladoEsquerda =:= 1, IsoladoDireita =:= 1, IsoladoAcima =:= 1, IsoladoAbaixo =:= 1 ->
        
        nb_getval( lista, Lista ),
        inseridoNoFinal([Coluna, Linha, ElementoAtual], Lista, NovaLista),
        nb_setval( lista, NovaLista )
    ;
        true
    ).


/* Dada as coordenadas 'X, Y' de uma Matriz, retorna o elemento ElementoObtido que se encontra 
 *   na esquerda dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_pixelsIsolados_ObterElementoEsquerda( X, Y, OutraMatriz, ElementoObtido ) :-
    
    Novo_X is X - 1,
    
    ( Novo_X > -1 -> 
    
	    dadoNaPosicao( LinhaAtual, OutraMatriz, Y ), 
	    dadoNaPosicao( ElementoObtido, LinhaAtual, Novo_X )
    ;
        true
    ).


/* Dada as coordenadas 'X, Y' de uma Matriz, retorna o elemento ElementoObtido que se encontra 
 *   na direita dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_pixelsIsolados_ObterElementoDireita( X, Y, Matriz, ElementoObtido ) :-
    
    nb_getval( larguraDaMatriz, LarguraDaMatriz ), 
    
    Novo_X is X + 1,
    
    ( Novo_X < LarguraDaMatriz -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Y ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, Novo_X )
    ;
        true
    ).


/* Dada as coordenadas 'X, Y' de uma Matriz, retorna o elemento ElementoObtido que se encontra 
 *   a cima dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_pixelsIsolados_ObterElementoAcima( X, Y, Matriz, ElementoObtido ) :-
    
    Novo_Y is Y - 1,
    
    ( Novo_Y > -1 -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Novo_Y ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, X )
    ;
        true
    ).


/* Dada as coordenadas 'X, Y' de uma Matriz, retorna o elemento ElementoObtido que se encontra 
 *   a baixo dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_pixelsIsolados_ObterElementoAbaixo( X, Y, Matriz, ElementoObtido ) :-
    
    nb_getval( alturaDaMatriz, AlturaDaMatriz ), 
    
    Novo_Y is Y + 1,
    
    ( Novo_Y < AlturaDaMatriz -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Novo_Y ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, X )
    ;
        true
    ).


%###################################### caminhoEntrePixels ########################################
/* Verificação de caminho entre dois pixels: há um caminho entre dois pixels, se há um conjunto 
 *   de pixels adjacentes subsequentes (considerando os quatro vizinhos), todos com intensidades 
 *   maiores do que zero, ligando estes dois pixels.
 * 
 * Dadas as posições de pixels X1, Y1, e X2 e Y2 dessa matriz, retorna uma lista Caminho
 *   contendo um lista de listas de pares de pixels que fazem parte do caminho entre os pixels 
 *   X e Y.
 * Para resolução, carrega-se em memória o grafo 'Trabalho3/grafo.pl' criado a partir da Matriz,  
 *   e se executa-se o algoritmo de caminhos em grafos, e obtem-se o caminho entre os pixels. 
 * */
caminhoEntrePixels( X1, Y1, X2, Y2, Caminho ) :-
    
    limparMemoria, 
    carregarGrafo( 'Trabalho3/grafo.pl' ), 
    
    privado_ObterElemento( X1, Y1, Matriz, X ), 
    privado_ObterElemento( X2, Y2, Matriz, Y ), 
    privado_viajarPeloGrafo([X1, Y1, X], [X2, Y2, Y], Caminho ), 
    
    nl, nl, 
    write('Caminho entre: '), write( X1 ), write( Y1 ), 
    write(' e '), write( X2 ), write( Y2 ), nl, nl, 
    write( Caminho ), nl, nl,
    !.


/* Dada uma Matriz constrói um grafo de conexões dos pixels e salva no arquivo 
 *   'Trabalho3/grafo.pl'.
 * */
caminhoEntrePixels_ConstruirGrafo( Matriz ):-
    
    dadoNaPosicao( PrimeiroElemento, Matriz, 0 ), 
    length( PrimeiroElemento, LarguraDaMatriz ), 
    length( Matriz, AlturaDaMatriz ), 
    
    nb_setval( matriz, Matriz ),
    nb_setval( larguraDaMatriz, LarguraDaMatriz ), 
    nb_setval( alturaDaMatriz, AlturaDaMatriz ), 
    nb_setval( coordenada_LinhaAtual, -1 ), 
    nb_setval( coordenada_ColunaAtual, -1 ), 
    
    limparMemoria, 
    carregarGrafo( 'Trabalho3/grafo.pl' ), 
    
    privado_caminhoEntrePixels_ComputarMatriz( Matriz ),

    gravarGrafo( 'Trabalho3/grafo.pl' ),
    !.


/* Lista todas as claúsulas e grava o grafo no Arquivo e limpa a memória.
 * */
gravarGrafo( Arquivo ) :-
        
    tell( Arquivo ), 
    listing(aresta), 
    listing(vertice),
    told,
    limparMemoria.


/* Carrega o grafo salvo no Arquivo em memória.
 * */
carregarGrafo( Arquivo ) :-
    consult( Arquivo ).


/* A failure-driven loop para passar em todas as linhas da Matriz.
 * */
privado_caminhoEntrePixels_ComputarMatriz( Matriz ) :- 

    member( LinhaAtual, Matriz ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    NovaCoordenada_LinhaAtual is Coordenada_LinhaAtual + 1, 
    nb_setval( coordenada_LinhaAtual, NovaCoordenada_LinhaAtual ), 
    nl, nl, 
    
    privado_caminhoEntrePixels_ComputarLinhas( LinhaAtual ), 
    fail.


    /* Faz a failure-driven loop 'privado_caminhoEntrePixels_ComputarMatriz' retornar true ao  
     *   invés de falhar.
     * */
    privado_caminhoEntrePixels_ComputarMatriz( _ ).


/* A failure-driven loop para passar em todos os elementos da linha da Matriz.
 * */
privado_caminhoEntrePixels_ComputarLinhas( LinhaAtual ) :- 
    
    member( ElementoAtual, LinhaAtual ), 
    
    nb_getval( coordenada_LinhaAtual, Coordenada_LinhaAtual ), 
    nb_getval( coordenada_ColunaAtual, Coordenada_ColunaAtual ), 
    nb_getval( larguraDaMatriz, LarguraDaMatriz ),
    NovaCoordenada_ColunaAtual is ( Coordenada_ColunaAtual + 1 ) mod LarguraDaMatriz, 
    nb_setval( coordenada_ColunaAtual, NovaCoordenada_ColunaAtual ),
    
    write( Coordenada_LinhaAtual ), write(','),
    write( NovaCoordenada_ColunaAtual ), write(','),
    write( ElementoAtual ), write('- '),
    
    privado_caminhoEntrePixels_ComputarElementos( 
                               NovaCoordenada_ColunaAtual, Coordenada_LinhaAtual, ElementoAtual ),
    fail.
    
    
    /* Faz a failure-driven loop 'privado_caminhoEntrePixels_ComputarLinhas' retornar true ao  
     *   invés de falhar.
     * */
    privado_caminhoEntrePixels_ComputarLinhas( _ ).


/* Executa o algoritmo de caminhoEntrePixels na LinhaAtual da ColunaAtual do ElementoAtual.
 * */
privado_caminhoEntrePixels_ComputarElementos( Coluna, Linha, ElementoAtual ) :-
    
    adicionarVertice( [Coluna, Linha, ElementoAtual] ), 
    nb_getval( matriz, Matriz ),
    
    privado_caminhoEntrePixels_ObterElementoEsquerda( Coluna, Linha, Matriz, Esquerda ), 
    
    ( nonvar( Esquerda ) -> 
        
        ( Esquerda > 0 ->
            
            Nova_Coluna1 is Coluna - 1, 
            adicionarVertice( [Nova_Coluna1, Linha, Esquerda] ),
            conectar( [Coluna, Linha, ElementoAtual], [Nova_Coluna1, Linha, Esquerda] )
        ;
            true
        )
    ;
        true
    ),
    privado_caminhoEntrePixels_ObterElementoDireita( Coluna, Linha, Matriz, Direita ),
    
    ( nonvar( Direita ) -> 
        
        ( Direita > 0 ->
            
            Nova_Coluna2 is Coluna + 1, 
            adicionarVertice( [Nova_Coluna2, Linha, Direita] ),
            conectar( [Coluna, Linha, ElementoAtual], [Nova_Coluna2, Linha, Direita] )
        ;
            true
        )
    ;
        true
    ),
    privado_caminhoEntrePixels_ObterElementoAcima( Coluna, Linha, Matriz, Acima ),
    
    ( nonvar( Acima ) -> 
       
        ( Acima > 0 ->
            
            Nova_Linha1 is Linha - 1,
            adicionarVertice( [Coluna, Nova_Linha1, Acima] ),
            conectar( [Coluna, Linha, ElementoAtual], [Coluna, Nova_Linha1, Acima] )
        ;
            true
        )
    ;
        true
    ),
    privado_caminhoEntrePixels_ObterElementoAbaixo( Coluna, Linha, Matriz, Abaixo ), 
    
    ( nonvar( Abaixo ) -> 

        ( ElementoAtual > Abaixo ->
            
            Nova_Linha2 is Linha + 1,
            adicionarVertice( [Coluna, Nova_Linha2, Abaixo] ),
            conectar( [Coluna, Linha, ElementoAtual], [Coluna, Nova_Linha2, Abaixo] )
        ;
            true
        )
    ;
        true
    ).


/* Dada as coordenadas 'Coluna, Linha' de uma Matriz, retorna o elemento ElementoObtido que se  
 *   encontra na esquerda dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_caminhoEntrePixels_ObterElementoEsquerda( Coluna, Linha, OutraMatriz, ElementoObtido ) :-
    
    Novo_Coluna is Coluna - 1,
    
    ( Novo_Coluna > -1 -> 
    
        dadoNaPosicao( LinhaAtual, OutraMatriz, Linha ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, Novo_Coluna )
    ;
        true
    ).


/* Dada as coordenadas 'Coluna, Linha' de uma Matriz, retorna o elemento ElementoObtido que se  
 *   encontra na direita dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_caminhoEntrePixels_ObterElementoDireita( Coluna, Linha, Matriz, ElementoObtido ) :-
    
    nb_getval( larguraDaMatriz, LarguraDaMatriz ), 
    
    Novo_Coluna is Coluna + 1,
    
    ( Novo_Coluna < LarguraDaMatriz -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Linha ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, Novo_Coluna )
    ;
        true
    ).


/* Dada as coordenadas 'Coluna, Linha' de uma Matriz, retorna o elemento ElementoObtido que se  
 *   encontra a cima dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_caminhoEntrePixels_ObterElementoAcima( Coluna, Linha, Matriz, ElementoObtido ) :-
    
    Novo_Linha is Linha - 1,
    
    ( Novo_Linha > -1 -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Novo_Linha ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, Coluna )
    ;
        true
    ).


/* Dada as coordenadas 'Coluna, Linha' de uma Matriz, retorna o elemento ElementoObtido que se  
 *   encontra a baixo dessa posição. Caso não seja possível, não inicializa o ElementoObtido.
 * */
privado_caminhoEntrePixels_ObterElementoAbaixo( Coluna, Linha, Matriz, ElementoObtido ) :-
    
    nb_getval( alturaDaMatriz, AlturaDaMatriz ), 
    
    Novo_Linha is Linha + 1,
    
    ( Novo_Linha < AlturaDaMatriz -> 
    
        dadoNaPosicao( LinhaAtual, Matriz, Novo_Linha ), 
        dadoNaPosicao( ElementoObtido, LinhaAtual, Coluna )
    ;
        true
    ).















