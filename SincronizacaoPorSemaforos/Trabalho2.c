/**
 * @authors Luiz Henrique Urias, Evandro  Coan
 */
/*
 * Serão duas threads,
 * o cliente será o produtor
 * o funcionário será o consumidor
 * E as cadeiras o buffer
 *
 * Um cliente tem que entrar sentar na cadeira, e somente depois um funcionário
 * pode retirar o cliente da cadeira.
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include "queue.h"
/**
 * Define o número total de clientes
 */
#define TOTAL_CLIENTES 10
/**
 * Define o número total de funcionários
 */
#define TOTAL_FUNCIONARIOS 2
/**
 * Define o tamanho da fila
 */
#define CADEIRAS 4
/**
 * Semáforo para fazer a sincronização entre o cliente e o funcionário
 */
sem_t semCliente[ TOTAL_CLIENTES ];

/**
 * Semáforo para não permitir que um cliente entre em uma cadeira se ela 
 * estiver cheia. Fazendo a comunicação entre os clientes e os funcionários 
 * dizendo: -Ocupei uma cadeira | -Liberei uma cadeira.
 */
sem_t cheio;
sem_t vazio;
/**
 * Semáforo binário para o acesso à região crítica.
 */
sem_t lock_Cliente;
sem_t lock_Funcionario;
/**
 * Define a fila onde estarão os clientes
 */
queue_t filaClientes;

/**
 * Função responsável por criar uma thread para representar um cliente
 * 
 * @param arg índice do cliente como um ponteiro void
 */
void *cliente( void* arg )
{
    long numThread = (long) arg;
    while( 1 )
    {
        if ( length( &filaClientes ) < CADEIRAS )
        {
            // bloqueia para que somente o número específico de cadeiras seja 
            // acessado
            sem_wait( &vazio );
            
            // bloqueia a fila para que mais de um cliente não entre ao mesmo
            // tempo na fila
            sem_wait( &lock_Cliente );
            
            enqueue( &filaClientes, numThread );
            printf( "\nCliente %ld: chegou"
                    " (%d/%d lugares ocupados)",
                    numThread, length( &filaClientes ), CADEIRAS );
            
            // desbloqueia a fila para que os clientes que aguardam possam 
            // entrar fila
            sem_post( &lock_Cliente );
            
            // (incrementa o semáforo, avisa aos funcionários que chegou um
            // cliente ). Permite que os funcionários chamem os clientes.
            sem_post( &cheio );
            
            //aguarda atendimento
            sem_wait( &semCliente[ numThread ] );
        } else
        {
            printf( "\nCliente %ld: cartorio lotado, saindo para dar "
                    "uma volta (%d/%d lugares ocupados)",
                    numThread, length( &filaClientes ), CADEIRAS );
        }
        
        sleep( 10 );
    }
}

/**
 * Função responsável por criar uma thread para representar um funcionário
 * 
 * @param arg índice do funcionário como um ponteiro void
 */
void *funcionario( void * arg )
{
    long numThread = (long) arg;
    while( 1 )
    {
        int id_cliente;
        
        // aguarda um cliente (enquanto o cheio for zero, este semáforo impede
        // que o funcionário chame alguém )
        sem_wait( &cheio );
        
        // bloqueia a fila para que dois funcionário não retirem clientes ao 
        // mesmo tempo
        sem_wait( &lock_Funcionario );
        
        id_cliente = dequeue( &filaClientes );
        printf( "\nFuncionario %ld: atendendo cliente %d "
                "(%d/%d lugares ocupados)",
                numThread, id_cliente, length( &filaClientes ), CADEIRAS );
        
        // desbloqueia a fila para outro funcionário passa retirar clientes
        sem_post( &lock_Funcionario );
        
        // avisa o cliente que liberou-se uma cadeira
        sem_post( &vazio );
        
        //simula atendimento
        sleep( 5 + ( rand() % 6 ) );
        printf( "\nFuncionario %ld: terminou de atender "
                "cliente %d (%d/%d lugares ocupados)",
                numThread, id_cliente, length( &filaClientes ), CADEIRAS );
        
        //libera o cliente
        sem_post( &semCliente[ id_cliente ] );
    }
}

/**
 * Função principal que inicializa a execução do programa
 * 
 * @param argc não utilizado
 * @param argv não utilizado
 */
void main( int argc, char **argv )
{
    //Cria uma semente de números diferente a cada segundo.
    srand( time( NULL ) );
    
    // inicializa a fila de clientes
    init_queue( &filaClientes );
    
    // Declara as threads cliente
    pthread_t threadsCliente[ TOTAL_CLIENTES ];
    
    // Declara as threads funcionário
    pthread_t threadsFuncionario[ TOTAL_FUNCIONARIOS ];
    
    // semáforo que bloqueia para que somente o número específico de 
    // cadeiras seja acessado
    sem_init( &vazio, 0, CADEIRAS );
    
    // (semáforo que avisa aos funcionários que chegou um
    // cliente ). Permite que os funcionários chamem os clientes.
    sem_init( &cheio, 0, 0 );
    
    // semáforo binário que bloqueia a fila para que os clientes que aguardam 
    // possam entrar fila
    sem_init( &lock_Cliente, 0, 1 );
    
    // semáforo binário que bloqueia a fila para que dois funcionário não 
    // retirem clientes ao mesmo tempo
    sem_init( &lock_Funcionario, 0, 1 );
    
    printf( "Processo principal iniciado.\n" );
    
    // loop para o for's
    int i;
    
    //for para semaforo de cada cliente
    for ( i = 0; i < TOTAL_CLIENTES; i++ )
    {
        sem_init( &semCliente[ i ], 0, 0 );
    }
    
    // for para clientes, criação
    for ( i = 0; i < TOTAL_CLIENTES; i++ )
    {
        pthread_create( &threadsCliente[ i ], NULL, cliente, (void *) i );
    }
    
    // for para funcionários, criação
    for ( i = 0; i < TOTAL_FUNCIONARIOS; i++ )
    {
        pthread_create( &threadsFuncionario[ i ], NULL, funcionario,
                        (void *) i );
    }
    
    //for para clientes, espera
    for ( i = 0; i < TOTAL_CLIENTES; i++ )
    {
        pthread_join( threadsCliente[ i ], NULL );
    }
    
    // for para funcionários, espera
    for ( i = 0; i < TOTAL_FUNCIONARIOS; i++ )
    {
        pthread_join( threadsFuncionario[ i ], NULL );
    }
    
    //destroi os semaforos
    sem_destroy( &cheio );
    sem_destroy( &vazio );
    sem_destroy( &lock_Cliente );
    sem_destroy( &lock_Funcionario );
    
    //fecha as threads
    pthread_exit( NULL );
}
