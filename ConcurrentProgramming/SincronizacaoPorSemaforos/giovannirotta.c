#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <semaphore.h>
#include "queue.h"

#define CADEIRAS 4
#define TOTAL_FUNCIONARIOS 2
#define TOTAL_CLIENTES 10


queue_t cadeiras_cartorio;
sem_t pos_vazia[TOTAL_CLIENTES];
sem_t pos_ocupada;
sem_t cliente_mutex;
sem_t funcionario_mutex;


void *func_funcionario(void *argumento) {
int id_funcionario = *((int *) argumento);
int id_cliente;
int n;
while (1) {
    sem_wait(&pos_ocupada);
    sem_wait(&funcionario_mutex);
      if ( length(&cadeiras_cartorio) > 0){
    id_cliente = dequeue(&cadeiras_cartorio);
    printf("Funcionario %d: atendendo cliente %d (%d/%d lugares ocupados)\n", id_funcionario, id_cliente, length(&cadeiras_cartorio), CADEIRAS);
    sem_post(&funcionario_mutex);    
    sleep(5 + rand() % 6);
    printf("Funcionario %d: terminou de atender cliente %d (%d/%d lugares ocupados)\n", id_funcionario, id_cliente, length(&cadeiras_cartorio), CADEIRAS);
    sem_post(&pos_vazia[id_cliente]);
    }else{
            sem_post(&funcionario_mutex); 
    sleep(10);
    }
    }//while
    pthread_exit(NULL);
    }


void *func_cliente(void *argumento) {
int id_cliente = *((int *) argumento);
int n;
while (1) {
            sem_wait(&cliente_mutex);
    if ( length(&cadeiras_cartorio) < CADEIRAS) {
    enqueue(&cadeiras_cartorio , id_cliente);
    printf("Cliente %d: chegou (%d/%d lugares ocupados)\n", id_cliente, length(&cadeiras_cartorio), CADEIRAS);
    sem_post(&cliente_mutex);
    sem_wait(&pos_vazia[id_cliente]);
    }
    else {
    printf("Cliente %d: cartorio lotado, saindo para dar uma volta (%d/%d lugares ocupados)\n", id_cliente, length(&cadeiras_cartorio), CADEIRAS);
      sem_post(&cliente_mutex);
    }
    sem_post(&pos_ocupada);
    sleep(10);
    }//while    
    pthread_exit(NULL);
    }


void criarPessoas(){

pthread_t threadsF[TOTAL_FUNCIONARIOS];
pthread_t threadsC[TOTAL_CLIENTES];
int f=0;
for(f; f < TOTAL_FUNCIONARIOS; f++){
            int *arg = malloc(sizeof(*arg));
        if ( arg == NULL ) {
                printf("Não foi possível alocar memória para o argumento da thread.\n");
                exit(EXIT_FAILURE);
                   }
        *arg = f;
        pthread_create(&threadsF[f], NULL, func_funcionario, arg);
    }
int c=0;
for(c; c < TOTAL_CLIENTES; c++){
        int *arg = malloc(sizeof(*arg));
        if ( arg == NULL ) {
                printf("Não foi possível alocar memória para o argumento da thread.\n");
                exit(EXIT_FAILURE);
                   }
        *arg = c;
        pthread_create(&threadsC[c], NULL, func_cliente, arg);
    }
int fj=0;
for(fj; fj < TOTAL_FUNCIONARIOS; fj++){
        pthread_join(threadsF[fj], NULL);    
    }
int cj=0;
for(cj; cj < TOTAL_CLIENTES; cj++){
        pthread_join(threadsC[cj], NULL);
    }

    pthread_exit(NULL);
}

int main(int argc, char **argv) {

    int i = 0;
    for(i; i < TOTAL_CLIENTES; i++);
      sem_init(&pos_vazia[i],0,1);
      sem_init(&pos_ocupada, 0, 0);
      sem_init(&cliente_mutex,0,1);
      sem_init(&funcionario_mutex,0,1);

    init_queue(&cadeiras_cartorio) ;
    srand(time(NULL));

    printf("Processo principal iniciado.\n");
    
    criarPessoas();

    int a = 0;
    for(a; a < TOTAL_CLIENTES; a++){
    sem_destroy(&pos_vazia[a]);
    }
      sem_destroy(&pos_ocupada);
    sem_destroy(&cliente_mutex);
sem_destroy(&funcionario_mutex);
}

