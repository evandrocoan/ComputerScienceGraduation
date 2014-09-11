/**
 * @authors Luiz Henrique Urias, Evandro  Coan
 */

/*
 * Serão duas threads,
 *  o cliente será o produtor
 *  o funcionário será o consumidor
 * E as cadeiras o buffer
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
 * Define o númeor total de funcionários
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
sem_t lock_prod;
sem_t lock_cons;

/**
 * Define a fila onde estarão os clientes
 */
queue_t filaClientes;

void *produtor(){
while (true) {
sem_wait(vazio);
sem_wait(lock_prod);
f = (f + 1) % N;
buffer[f] = rand() %10;
sem_post(lock_prod);
sem_post(cheio);
}

void *consumidor(){
while (true) {
sem_wait(cheio);
sem_wait(lock_cons);
i = (i + 1) % N;
buffer[i] = -1;
sem_post(lock_cons);
sem_post(vazio);
}



int main(int argc, char **argv) 
{
    // inicializa a fila de clientes
    init_queue( &filaClientes );
    
    pthread_t threadsCliente[TOTAL_CLIENTES];
    pthread_t threadsFuncionario[TOTAL_FUNCIONARIOS];
    
    sem_init(&sem, 0,1);

    printf("Processo principal iniciado.\n");

    for(int i=0; i < MAX_THREADS; i++){
        pthread_create(&threadsConsumidoras[i], NULL, produtor, NULL)
        pthread_create(&threadsProdutoras[i],NULL, consumidor,NULL)}

    for(int i=0; i < MAX_THREADS; i++){
        pthread_join(threadsConsumidoras[i], NULL);
        pthread_join(threadsProdutoras[i], NULL);}

    printf("Processo principal: var_compartilhada = %d.\n", var_compartilhada);

    sem_destroy(&sem);
    pthread_exit(NULL);
    return 0;
}
