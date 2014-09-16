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
void *cliente(void* arg) {
	long numThread = (long) arg;
	while (1) {
		if (length(&filaClientes) < CADEIRAS) {
			sem_wait(&vazio);
			sem_wait(&lock_Cliente);
			enqueue(&filaClientes, numThread);
			printf("\nCliente %d: chegou"
					" (%d/%d lugares ocupados)", numThread,
					length(&filaClientes), CADEIRAS);
			sem_post(&lock_Cliente);
			sem_post(&cheio);
		} else {
			printf("\nCliente %d: cartorio lotado, saindo para dar "
					"uma volta (%d/%d lugares ocupados)", numThread,
					length(&filaClientes), CADEIRAS);
		}
		sleep(10);
	}
}
void *funcionario(void * arg) {
	long numThread = (long) arg;
	while (1) {
		if (length(&filaClientes)) {
			int id_cliente;
			sem_wait(&cheio);
			sem_wait(&lock_Funcionario);
			id_cliente = dequeue(&filaClientes);
			printf("\nFuncionario %d: atendendo cliente %d "
					"(%d/%d lugares ocupados)", numThread, id_cliente,
					length(&filaClientes), CADEIRAS);
			sem_post(&lock_Funcionario);
			sleep(5 + (rand() % 6));
			printf("\nFuncionario %d: terminou de atender "
					"cliente %ld (%d/%d lugares ocupados)", numThread,
					id_cliente, length(&filaClientes), CADEIRAS);
			sem_post(&vazio);
		}
	}
}
void main(int argc, char **argv) {
//Cria uma semente de números diferente a cada segundo.
	srand(time(NULL));
// inicializa a fila de clientes
	init_queue(&filaClientes);
	pthread_t threadsCliente[TOTAL_CLIENTES];
	pthread_t threadsFuncionario[TOTAL_FUNCIONARIOS];
//inicializa os semaforos
	sem_init(&vazio, 0, CADEIRAS);
	sem_init(&cheio, 0, 0);
	sem_init(&lock_Cliente, 0, 1);
	sem_init(&lock_Funcionario, 0, 1);
	printf("Processo principal iniciado.\n");
// loop para o for's
	int i;
// for para clientes, criação
	for (i = 0; i < TOTAL_CLIENTES; i++) {
		pthread_create(&threadsCliente[i], NULL, cliente, (void *) i);
	}
// for para funcionários, criação
	for (i = 0; i < TOTAL_FUNCIONARIOS; i++) {
		pthread_create(&threadsFuncionario[i], NULL, funcionario, (void *) i);
	}
//for para clientes, espera
	for (i = 0; i < TOTAL_CLIENTES; i++) {
		pthread_join(threadsCliente[i], NULL);
	}
// for para funcionários, espera
	for (i = 0; i < TOTAL_FUNCIONARIOS; i++) {
		pthread_join(threadsFuncionario[i], NULL);
	}
//destroi os semaforos
	sem_destroy(&vazio);
	sem_destroy(&cheio);
	sem_destroy(&lock_Cliente);
	sem_destroy(&lock_Funcionario);
//fecha as threads
	pthread_exit(NULL);
}
