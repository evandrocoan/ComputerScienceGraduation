/**
 * @authors Luiz Henrique Urias, Evandro  Coan
 */

/*
 * Serão duas threads,
 *  o cliente será o produtor
 *  o funcionário será o consumidor
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
#include <queue.h>

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
 * 
 */
sem_t cheio;
sem_t vazio;



/**
 * 
 */
sem_t lock_Cliente;
sem_t lock_Funcionario;

/**
 * Define a fila onde estarão os clientes
 */
queue_t filaClientes;

void *cliente()
{
    while (1) 
    {
        sem_wait(vazio);
        sem_wait(lock_Cliente);
        f = (f + 1) % N;
        buffer[f] = rand() %10;
        sem_post(lock_Cliente);
        sem_post(cheio);
    }
}

void *funcionario()
{
    
    while (1) 
    {
        sem_wait(cheio);
        sem_wait(lock_Funcionario);
        i = (i + 1) % N;
        buffer[i] = -1;
        sem_post(lock_Funcionario);
        sem_post(vazio);
    }
}



int main(int argc, char **argv) 
{
    // inicializa a fila de clientes
    init_queue( &filaClientes );
    
    pthread_t threadsCliente[TOTAL_CLIENTES];
    pthread_t threadsFuncionario[TOTAL_FUNCIONARIOS];
    
    sem_init(&, 0,1);

    printf("Processo principal iniciado.\n");

    // for para funcionários, criação
    for(int i=0; i < TOTAL_FUNCIONARIOS; i++)
    {
        pthread_create(&threadsFuncionario[i],NULL, funcionario,NULL)
    }
    
    // for para clientes, criação
    for(int i=0; i < TOTAL_CLIENTES; i++)
    {
        pthread_create(&threadsCliente[i], NULL, cliente, NULL);
    } 

    // for para funcionários, espera
    for(int i=0; i < TOTAL_FUNCIONARIOS; i++)
    {
        pthread_join(threadsFuncionario[i], NULL);)
    }
    
    // for para clientes, espera
    for(int i=0; i < TOTAL_CLIENTES; i++)
    {
        pthread_join(threadsCliente[i], NULL);
    } 

    printf("Processo principal: var_compartilhada = %d.\n", var_compartilhada);

    sem_destroy(&sem);
    pthread_exit(NULL);
    return 0;
}
