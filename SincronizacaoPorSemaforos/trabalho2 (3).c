#include "queue.h"
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


#define CADEIRAS 4
#define TOTAL_FUNCIONARIOS 2
#define TOTAL_CLIENTES 10

queue_t fila;
sem_t semClienteBin;
sem_t semFuncionarioBin;
sem_t semCadeiras;
sem_t semCliente[TOTAL_CLIENTES];

void *func_cliente(void *argumento) {
	int idCliente =*((int*)argumento);
	while(1){
	sem_wait(&semClienteBin);
	if(length(&fila)<CADEIRAS){
	printf("Cliente %d: chegou (%d/%d lugares ocupados)\n",idCliente,length(&fila),CADEIRAS);
	enqueue(&fila,idCliente);
	sem_post(&semClienteBin);
	//aguarda ser chamado pelo func
	sem_post(&semCadeiras);
	sem_wait(&semCliente[idCliente]);
	}
	else {
	printf("Cliente %d: cartorio lotado, saindo para dar uma volta (%d/%d lugares ocupados)\n",idCliente,length(&fila),CADEIRAS);
	sem_post(&semClienteBin);
	sleep(rand() % 5);//dar uma volta
	}
	sleep (10);
	}
	pthread_exit(NULL);
}

void *func_funcionario(void *argumento) {
	int idFuncionario = *((int*)argumento);
	while(1){
	//aguarda um cliente
	sem_wait(&semCadeiras);
	sem_wait(&semFuncionarioBin);
	int idCliente = dequeue(&fila);
	printf("Funcionario %d: atendendo cliente %d (%d/%d lugares ocupados)\n",idFuncionario,idCliente,length(&fila),CADEIRAS);
	sem_post(&semFuncionarioBin);
	sleep (5 + rand() % 6);//tempo de atendimento de um cliente
	printf("Funcionario %d: terminou de atender cliente %d (%d/%d lugares ocupados)\n",idFuncionario,idCliente,length(&fila),CADEIRAS);
	sem_post(&semCliente[idCliente]);
	sleep (5 + rand() % 5);
	//libera cliente do atendimento
	}
	pthread_exit(NULL);
}

int main(int argc, char **argv) {
	srand(time(NULL));
	pthread_t clientes[TOTAL_CLIENTES];
	pthread_t funcionarios[TOTAL_FUNCIONARIOS];
	init_queue(&fila);
	sem_init(&semClienteBin,0,1);
	sem_init(&semFuncionarioBin,0,1);
	sem_init(&semCadeiras,0,0);

	int i;

	for(i=0; i < TOTAL_CLIENTES; i++);
	sem_init(&semCliente[i],0,1);

	int* clienteID;
	clienteID = (int*)malloc(sizeof(int)*TOTAL_CLIENTES);

	int* funcionarioID;
	funcionarioID = (int*)malloc(sizeof(int)*TOTAL_FUNCIONARIOS);

	for(i=0; i < TOTAL_CLIENTES; i++){
	clienteID[i] = i;
	pthread_create(&clientes[i], NULL, func_cliente, &clienteID[i]);
	}

	for(i=0; i < TOTAL_FUNCIONARIOS; i++){
	funcionarioID[i] = i;
	pthread_create(&funcionarios[i], NULL, func_funcionario, &funcionarioID[i]);
	}

	for(i=0; i < TOTAL_CLIENTES; i++)
	pthread_join(clientes[i], NULL);

	for(i=0; i < TOTAL_FUNCIONARIOS; i++)
	pthread_join(funcionarios[i], NULL);

	
	for(i=0; i < TOTAL_CLIENTES; i++);
	sem_destroy(&semCliente[i]);
	sem_destroy(&semClienteBin);
	sem_destroy(&semCadeiras);
	sem_destroy(&semFuncionarioBin);
	free(clienteID);
	free(funcionarioID);
}
