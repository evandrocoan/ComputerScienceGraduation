//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/*
 * Projeto I: Simulação de Tráfego
 * 
 * Uma simulação via texto, com alguns parâmetros como:
 * 'o tempo de chegada de novos carros' de forma a existir a possibilidade 
 * de ocorrer "retrobloqueio" de um cruzamento em função de acúmulo de carros 
 * no trecho após o sinal.
 * 
 * 
 * Requisitos Elicitados
 * 
 * Pistas
 * 
 * 1. Representar cada pista por uma fila
 * 
 * 1.1 O sistema possui uma lista de suas filas
 * 
 * 1.2 Cada fila contem uma lista das filas de saída e uma velocidade
 * 
 * 1.3 Cada fila possui umna velocidade
 * 
 * 1.4 Carros saem de uma fila para ir para outra com a velocidade da fila 
 * destino
 * 
 * 1.5 Cada fila tem um tamanho fixo em metros e comporta um número limitado 
 * de carros.
 * 
 * 1'.6 Quando uma pista enche, a entrada daquela pista é bloqueada e o 
 * semáforo é marcado
 * 
 * 1.7 Se o próximo carro de uma pista estiver programado para entrar em uma 
 * pista cheia, ele não entra e bloqueia a pista.
 * 
 * 1.8 Cada pista tem uma variável randomica com distribuição uniforme 
 * dividida em faixas de valores que modela para qual de suas pistas 
 * eferentes um carro vai ir.
 * 
 * 1.9 Algumas filas são sumidouros e carros que nela entram, são eliminados 
 * após a percorrerm.
 * 
 * 1.10 Algumas filas são fontes e "recebem" carros a intervalos randômicos 
 * dentro de uma faixa de tempos com média e faixa de valores definida 
 * pelo professor em aula (PPT).
 * 
 * 
 * Semáforo
 * 
 * 2. O sistema possui uma lista dos semáforos
 * 
 * 2.1 Cada semáforo possui uma lista das filas que fazem parte dele, 
 * dividindo-as em eferentes (saída) e aferentes (entrada).
 * 
 * 2.2 Cada semáforo asocia à lista de "pistas eferentes" (as suas filas 
 * de sáida) uma lista de probabilidades de um carro dobrar em cada uma 
 * dessas pistas eferentes.
 * 
 * 
 * Veículos
 * 
 * 3. Cada veículo possui um tamanho
 * 
 * 3.1. O tamanho do veículo é dado pelo seu tamanho mais 1 metro à 
 * frente e 2 metros atrás.
 * 
 * 
 * Relógio
 * 
 * 4. O sistema possui uma lista de eventos que representa o "relógio do 
 * sistema".
 * 
 * 
 * 4.1: São eventos: 
 * 
 * - chegada de um novo carro
 * - mudança de estado do semáforo
 * - chegada de carro ao ao semáforo
 * - troca de pista 
 * 
 * 4.2 O relógio é uma lista ordenada por hora de ocorrência do evento.
 * 
 * 
 * Geração de Valores Aleatórios
 * 
 * Este é um trabalho de aula e por isso devemos fazer alguns compromissos 
 * para que o tamanho do trabalho fique dentro de limites factíveis. 
 * Podemos imaginar que uma distribuição realista para o intervalo de 
 * tempo de chegada de carros é uma variável aleatória com distribuição normal.
 * 
 * Então não vamos complicar onde não há necessidade. O importante é 
 * aprender a programar uma simulação, e não obter dados absolutamente 
 * realistas. Para facilitar vamos então utilizar variáveis com 
 * distribuição uniforme.
 * 
 * Toques de programação para geração de valores aleatórios em um intervalo:
 * 
 * Gerar valores aleatorios com distribuição uniforme no intervalo 0 a 1, 
 * utilize as funções rand e srand.
 * 
 * Lembre-se de inicializar o sempre gerador de numeros aleatorios, 
 * antes de usar, para garantir de que sejam usados valores diferentes 
 * em cada simulação.
 * 
 * Para gerar um numero entre 0 e 1, voce precisa dividir o valor 
 * gerado por RAND_MAX, definido em stdlib.h.
 * 
 * Para gerar um numero aleatorio com distribuição uniforme em um 
 * intervalo, pegue este resultado, multiplique pelo tamanho do 
 * intervalo e adicione a valor do limite inferior do intervalo. 
 * Por exemplo: para gerar um valor aleatorio de tempos de chegada 
 * entre 8 e 12 segundo (10 +/- 2), você pega o tamanho do intervalo, 
 * que é de 8 a 12 inclusive, logo 5 valores, e multiplica o seu número 
 * aleatorio de 0 a 1 por 5. A seguir adiciona o limite inferior do 
 * intervalo, 8, ao resultado. Para que voce possa usar este numero 
 * ainda falta truncar, pegando so a parte inteira. Para isto basta 
 * fazer um typecasting: inteiro = (int) real;
 */

/**
 * Responsável pelas operações de entrada e saída de informações
 */
#include <iostream>

/**
 * Torna padrão os meios de entrada e saida
 */
using namespace std;

/* printf, NULL */
#include <stdio.h>      

/* srand, rand */
#include <stdlib.h>    

/* time */
#include <time.h>      

//----------------------------------------------------------------- 

// filas: 
// | | | | 
// 500m v| |^ 500m 500m v| |^ 500m 
//------------- ---------------------- -------------- 
// 2000m <- <- 300m <- <- 400m 
// 2000m -> -> 300m -> -> 400m 
//------------- ---------------------- -------------- 
// 500m v| |^ 500m 500m v| |^ 500m 
// | | | | 

// menor comprimento de veiculo = 2m (deve existir 3m entre os carros) 
//determina o tamanho da fila para o pior caso 

//ORGANIZACAO NUMÉRICA DAS PISTAS 
/* 
 6 5 13 12 
 --- 7 4 ------ 4 11 ------- 
 --- 0 3 ------ 3 10 ------- 
 1 2 8 9 
 */
//----------------------------------------------------------------- 
//Organização dos dados conforme numeração das pistas - dados das pistas 
int velocidades[ 14 ] =
{ 22, 17, 17, 17, 17, 17, 17, 22, 11, 11, 8, 8, 11, 11 };

int comprimento[ 14 ] =
{ 2000, 500, 500, 300, 300, 500, 500, 2000, 500, 500, 400, 400, 500, 500 };

//fonte = freq. de geracao dos carros, entrada na pista {x,+-y} segundos 
int fonte[ 14 ][ 2 ] =
{
{ 10, 2 },
  { 0, 0 },
  { 30, 7 },
  { 0, 0 },
  { 0, 0 },
  { 0, 0 },
  { 20, 5 },
  { 0, 0 },
  { 0, 0 },
  { 60, 15 },
  { 0, 0 },
  { 20, 5 },
  { 0, 0 },
  { 20, 5 } };

string nomesPista[ 14 ] =
{ "O1 Leste", "S1 Sul", "S1 Norte", "C1 Leste", "C1 Oeste", "N1 Norte",
  "N1 Sul", "O1 Oeste", "S2 Sul", "S2 Norte", "L1 Leste", "L1 Oeste",
  "N2 Norte", "N2 Sul" };

//direcao indica, na ordem anti-horaria, os possiveis caminhos a serem seguidos 
//ex.: pista[0] pode ir para pista 1 3 ou 5 
int direcao[ 14 ][ 3 ] =
{
{ 1, 3, 5 },
  { 1, 1, 1 },
  { 3, 5, 7 },
  { 8, 10, 12 },
  { 5, 7, 1 },
  { 5, 5, 5 },
  { 7, 1, 3 },
  { 7, 7, 7 },
  { 8, 8, 8 },
  { 10, 12, 4 },
  { 10, 10, 10 },
  { 12, 4, 8 },
  { 12, 12, 12 },
  { 4, 8, 10 } };

//ordem de porcentagem de direcao conforme ordem de direcao[14][3] 
int porctDirecao[ 14 ][ 3 ] =
{
{ 10, 80, 10 },
  { 100, 100, 100 },
  { 80, 10, 10 },
  { 30, 40, 30 },
  { 30, 40, 30 },
  { 100, 100, 100 },
  { 10, 10, 80 },
  { 100, 100, 100 },
  { 100, 100, 100 },
  { 40, 30, 30 },
  { 100, 100, 100 },
  { 40, 30, 30 },
  { 100, 100, 100 },
  { 30, 30, 40 } };

//------------------------------dados para o sistema ----------------------- 
//
/**
 * Moto, carro, caminhonete, onibus/caminhao (2 é distancia de seguranca 
 * ao carro da frente) 
 */
int tamVeiculo[ 4 ] =
{ 2 + 2, 4 + 2, 6 + 2, 14 + 2 };

/**
 * 15% moto, 70% carro, 10% caminhonete e 5% onibus/caminhao 
 */
int porctVeiculo[ 4 ] =
{ 15, 70, 10, 5 };
//pistas sumidouro 
int sumidouro[ 6 ] =
{ 1, 5, 7, 8, 10, 12 };

int abertasSinalPistas[ 4 ][ 2 ] =
{
{ 0, 3 },
  { 2, 9 },
  { 4, 11 },
  { 6, 13 } };
//no 1o tempo das sinaleiras as pistas 0 e 3 podem andar 
//no 2o tempo das sinaleiras as pistas 2 e 9 podem andar 
//no 3o tempo das sinaleiras as pistas 4 e 11 podem andar 
//no 4o tempo das sinaleiras as pistas 6 e 13 podem andar 

//-------------------------------------------------------------------------------------- 
struct Pistas
{
    int veiculo[ 500 ]; //máximo numero possivel de veiculos (2000 m/ (2m + 2m)) 
    int velocidade;
    int nrOutVeiculos;
    int nrVeiculos;
    int freqIn[ 2 ];
    int tamanho;
    int destino[ 3 ];
    int probDirecao[ 3 ];
    string nome;

    int somaTamanhos; //somatorio do tamanho dos veiculos 
    int inicio; //indica a posicao de inicio da fila de veiculos 
    int fim; //indica a posicao de fim da fila de veiculos 
    bool cheia; //indica se a pista esta cheia 
};

static Pistas pista[ 14 ]; //estrutura com as 14 pistas 

//---------------------------------------------------------------------------------- 
void inicializaPistas()
{
    for( int i = 0; i < 14; i++ )
    {
        pista[ i ].velocidade = velocidades[ i ];
        pista[ i ].nrOutVeiculos = 0;
        pista[ i ].nrVeiculos = 0;
        
        pista[ i ].freqIn[ 0 ] = fonte[ i ][ 0 ];
        pista[ i ].freqIn[ 1 ] = fonte[ i ][ 1 ];
        
        pista[ i ].tamanho = comprimento[ i ];
        
        pista[ i ].destino[ 0 ] = direcao[ i ][ 0 ];
        pista[ i ].destino[ 1 ] = direcao[ i ][ 1 ];
        pista[ i ].destino[ 2 ] = direcao[ i ][ 2 ];
        
        pista[ i ].probDirecao[ 0 ] = porctDirecao[ i ][ 0 ];
        pista[ i ].probDirecao[ 1 ] = porctDirecao[ i ][ 1 ];
        pista[ i ].probDirecao[ 2 ] = porctDirecao[ i ][ 2 ];
        
        pista[ i ].nome = nomesPista[ i ];
        pista[ i ].somaTamanhos = 0;
        pista[ i ].inicio = 0;
        pista[ i ].fim = 0;
        pista[ i ].cheia = false;
    }
}
//---------------------------------------------------------------------------------- 
void colocaVeiculo( int p, int tam ) //p = pista, tam = tamanho do carro a entrar na pista 
{
    int espaco = pista[ p ].tamanho - ( pista[ p ].somaTamanhos + tam );
    
    if( espaco >= 0 ) //se tem espaço coloca veiculo 
    {
        if( pista[ p ].fim == 500 ) //se chegou ao final do vetor volta a escrever no inicio (buffer circular) 
            pista[ p ].fim = 0;
        
        pista[ p ].veiculo[ pista[ p ].fim ] = tam; //coloca o carro na fila (armazena o tamanho do carro) 
        pista[ p ].nrVeiculos++;
        pista[ p ].fim++;
        pista[ p ].somaTamanhos += tam; //armazena o novo tamanho total da fila (coloca carro na fila 
                
        if( espaco == 0 )
            pista[ p ].cheia = true;
        else
            pista[ p ].cheia = false;
    } else
        //senão, nao faz nada, despreza o veiculo de entrada! 
        pista[ p ].cheia = true;
    
}
//----------------------------------------------------------------------------------- 
int retiraVeiculo( int p ) // acredito que o fim nao ultrapassa o inicio, temporalmente, pq tem o tamanho maximo como limite 
{ // e o inicio nao ultrapassa o fim porque so retira veiculo se exister 
    int tamVeic;
    
    if( pista[ p ].nrVeiculos > 0 ) //so retira veiculo se existir 
    {
        pista[ p ].nrVeiculos--;
        
        if( pista[ p ].inicio == 500 )
        {
            pista[ p ].inicio = 0;
        }
        
        tamVeic = pista[ p ].veiculo[ pista[ p ].inicio ]; //le o tamanho do veiculo 
                
        pista[ p ].somaTamanhos -= tamVeic; //retira o carro do inicio da fila 
        pista[ p ].inicio++;
        
        pista[ p ].nrOutVeiculos++;
        
        //cout << "tam Veicul" << tamVeic << endl; 
        
        return tamVeic; //retorna o tamanho do carro retirado da pista 
        
    }
    //cout << "tam Veicul" << 0 << endl; 
    return 0;
}

//---------------------------------------------------------------------------------- 
int sorteioFreq( int p ) //sorteio de um numero inteiro entre max e min - uso para a frequencia de geracao dos veiculos 
{
    int min, max;
    //ex. freqIn[2]={10,2} 10 +- 2 
    min = pista[ p ].freqIn[ 0 ] - pista[ p ].freqIn[ 1 ]; //10 - 2 = 8 
    max = ( 2 * pista[ p ].freqIn[ 1 ] ) + 1; // 4 + 1 = 5 
    
    return rand( ) % max + min; //x = rand() % 5 + 8; //x entre 8-12 
}
//---------------------------------------------------------------------------------- 
//funcao para sorteio de valores com determinada probabilidade, empregando o Método de Monte Carlo 
//não confere se entrou com dados errados! 
//nrClasses deve ser o número de probabilidades em prob[], as quais somadas devem resultar em 100! 
int mmc( int nrClasses, int prob[ ] )
{
    int x = rand( ) % 100 + 1; //sorteio de um nr inteiro entre 1 e 100 
    
    int valorComp[ nrClasses ]; //vetor para valores de comparação do Método de Monte Carlo 
    
    valorComp[ 0 ] = prob[ 0 ]; //o primeiro valor de comparacao é a primeira probabilidade 
            
    for( int i = 1; i < nrClasses; i++ )
    {
        valorComp[ i ] = valorComp[ i - 1 ] + prob[ i ]; //soma as faixas de comparação 
    }
    
    for( int j = 0; j < nrClasses; j++ )
    {
        if( x <= valorComp[ j ] )
            return j;
    }
    return -1; //nunca deve chegar aqui. 
}
//---------------------------------------------------------------------------------- 
void retiraVeiculosSumidouros() //retira todos os carros necessários dos sumidouros 
{
    int x;
    
    for( int i = 0; i < 6; i++ )
    {
        x = pista[ sumidouro[ i ] ].velocidade; //tamanho que tem que ser retirado (veiculos conforme tamanho)- m/s 
        
        while( ( pista[ sumidouro[ i ] ].nrVeiculos > 0 ) && ( x > 0 ) ) //pode retirar veiculos resultando em tamanhos maiores que x 
        {
            x = x - retiraVeiculo( sumidouro[ i ] );
            
            //cout << "x = " << x << " sumidouro " << pista[sumidouro[i]].nrVeiculos << endl; 
        }
    }
}
//---------------------------------------------------------------------------------- 
bool moveVeiculoPista( int pF )
{
    int xF = pista[ pF ].velocidade; //leitura da velocidade da pista fonte (para saber quantos veiculos devem ser movidos) 
    int pD, tamV; //pista destino, tamanho veiculo retirado 
            
    do
    {
        pD = pista[ pF ].destino[ mmc( 3, pista[ pF ].probDirecao ) ]; //sorteio da pista destino 
        
        if( ( pista[ pF ].nrVeiculos > 0 ) && ( pista[ pD ].cheia == false ) ) //se tem veiculos a retirar e destino tem espaco 
        {
            tamV = retiraVeiculo( pF );
            colocaVeiculo( pD, tamV );
            xF = xF - tamV;
            
            //cout << "Cheguei Aqui!" << xF << endl; 
        } else
        {
            if( pista[ pD ].cheia )
                cout << "Travou na Pista" << pD << endl;
            return false;
        }
        
    } while( xF > 0 );
    
    return true;
}
//---------------------------------------------------------------------------------- 
int main()
{
    int tSimulacao = 100000; //tempo da simulacao em segundos 
    int tAberturaSinal = 100; //tempo de abertura das sinaleiras em segundos 
    
    int contSinal = 0;
    int qualSinal = 0;
    
    //------------------------------------------------------------------------------ 
    inicializaPistas( ); //inicializacao das pistas 
    //------------------------------------------------------------------------------ 
    
    //------------------------------------------------------------------------------ 
    //Geracao de todos os tempos de chegada das pistas 
    int tamVt = tSimulacao / 8; //pior caso para o tamanho do vetor, um carro gerado a cada 8 s 
    int vt[ 6 ][ tamVt ]; //sao 6 pistas que recebem carros 
    int tCompAtual[ 6 ]; //vetor com o valor atual de comparacao para geracao dos veiculos atualizado a cada geracao 
    int indiceCompAtual[ 6 ] =
    { 0, 0, 0, 0, 0, 0 };
    int tamV;
    
    for( int i = 0; i < tamVt; i++ )
    { //pista que sao fontes de carros (entrada de carros no sistema) 
        vt[ 0 ][ i ] = sorteioFreq( 0 ); //pista 0 
        vt[ 1 ][ i ] = sorteioFreq( 2 ); //pista 2 
        vt[ 2 ][ i ] = sorteioFreq( 6 ); //pista 6 
        vt[ 3 ][ i ] = sorteioFreq( 9 ); //pista 9 
        vt[ 4 ][ i ] = sorteioFreq( 11 ); //pista 11 
        vt[ 5 ][ i ] = sorteioFreq( 13 ); //pista 13 
                
    } //todos os tempos de geracao de carros estao armazenados conforme aleatoriedade (muitos nao serao utilizados) 
    for( int i = 0; i < 6; i++ ) //inicializacao do primeiro tempo de geracao 
        tCompAtual[ i ] = vt[ i ][ 0 ];
    //------------------------------------------------------------------------------- 
    
    //Show Time! 
    
    for( int t = 0; t < tSimulacao; t++ )
    {
        
        //---------------------------------------------------------------------------- 
        //geracao dos carros 
        for( int n = 0; n < 6; n++ ) //verifica o tempo de geracao para as 6 pistas que fornecem carros 
        {
            if( tCompAtual[ n ] == t ) //se o tempo de geracao da pista n for igual ao da simulacao, gera carro 
            {
                tamV = tamVeiculo[ mmc( 4, porctVeiculo ) ]; //sorteia o tamanho do veiculo conforme porcentagens 
                switch( n )
                {
                    case 0:
                        colocaVeiculo( 0, tamV );
                        break;
                    case 1:
                        colocaVeiculo( 2, tamV );
                        break;
                    case 2:
                        colocaVeiculo( 6, tamV );
                        break;
                    case 3:
                        colocaVeiculo( 9, tamV );
                        break;
                    case 4:
                        colocaVeiculo( 11, tamV );
                        break;
                    case 5:
                        colocaVeiculo( 13, tamV );
                        break;
                }
                indiceCompAtual[ n ]++;
                tCompAtual[ n ] = tCompAtual[ n ]
                        + vt[ n ][ indiceCompAtual[ n ] ]; //pega o proximo tempo de comparacao 
            }
        }
        //-------------------------------------------------------------------------- 
        //contagem do tempo para a troca do sinal nos Semáforos 
        if( contSinal == tAberturaSinal )
        {
            contSinal = 0;
            qualSinal++;
            if( qualSinal == 4 )
                qualSinal = 0;
        }
        contSinal++;
        //--------------------------------------------------------------------------- 
        
        //cout << "Cheguei Aqui!" << endl; 
        
        retiraVeiculosSumidouros( );
        
        //abertura dos semáforos 
        switch( qualSinal )
        {
            //abertura das pistas 0 e 3 
            case 0:
                moveVeiculoPista( 0 );
                moveVeiculoPista( 3 );
                break;
                
                //abertura das pistas 2 e 9 
            case 1:
                moveVeiculoPista( 2 );
                moveVeiculoPista( 9 );
                break;
                
                //abertura das pistas 4 e 11 
            case 2:
                moveVeiculoPista( 4 );
                moveVeiculoPista( 11 );
                break;
                
                //abertura das pistas 6 e 13 
            case 3:
                moveVeiculoPista( 6 );
                moveVeiculoPista( 13 );
                break;
        }
        //-------------------------------------------------------------------------- 
        
    } //for SIMULACAO 
      //------------------------------------------------------------------------------ 
    
      //imprime nr de veiculos que deixaram o sistema 
    
    int total = 0;
    for( int i = 0; i < 6; i++ )
    {
        cout << "Nr veiculos que deixaram a pista [" << sumidouro[ i ] << "] = "
             << pista[ sumidouro[ i ] ].nrOutVeiculos << endl;
        total += pista[ sumidouro[ i ] ].nrOutVeiculos;
    }
    cout << "Nr total de veiculos que deixaram o sistema = " << total << endl;
    
    return 0;
}
//---------------------------------------------------------------------------------- 
