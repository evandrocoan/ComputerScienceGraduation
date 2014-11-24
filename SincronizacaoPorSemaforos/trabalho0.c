#include "queue.h"
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


#define CADEIRAS 4
#define TOTAL_FUNCIONARIOS 2
#define TOTAL_CLIENTES 10

queue_t fila;

void *func_cliente(void *argumento) {
	int idCliente =*((int*)argumento);
	printf("cliente id: %d\n",idCliente);
	enqueue(&fila,idCliente);
	int elemento = dequeue(&fila);
	printf("cliente id fila : %d\n",elemento);
	while (1) {
	if(length(&fila)<CADEIRAS){
	enqueue(&fila,idCliente);
	int elemento = dequeue(&fila);
	printf("cliente id fila : %d\n",elemento);
	//aguarda ser chamado pelo func
	//atendido pelo func
	}
	else {
	//dar uma volta
	}
	sleep (10);
	}
	pthread_exit(NULL);
}

int main(int argc, char **argv) {
	srand(time(NULL));
	pthread_t clientes[TOTAL_CLIENTES];
	init_queue(&fila);
	int* threadID;
	threadID = (int*)malloc(sizeof(int)*TOTAL_CLIENTES);

	int i;
	for(i=0; i < TOTAL_CLIENTES; i++){
	threadID[i] = i;
	pthread_create(&clientes[i], NULL, func_cliente, &threadID[i]);
	}

	for(i=0; i < TOTAL_CLIENTES; i++)
	pthread_join(clientes[i], NULL);

}
