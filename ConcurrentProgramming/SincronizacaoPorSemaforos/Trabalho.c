#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <semaphore.h>
#include "queue.h"

#define CADEIRAS 4
#define TOTAL_FUNCIONARIOS 2
#define TOTAL_CLIENTES 10 

sem_t full, sEmpty, lock_f, lock_c, *lock_cf; 
queue_t queue;
void *pCliente(void *a) {
	unsigned int n= (unsigned int) a;
	while(1) {
		sem_wait(&lock_c);  // região crítica pra entrada na fila
		if (length(&queue)< CADEIRAS) {
			enqueue(&queue, n);  // se tem espaço na fila o cliente entra
			printf("Cliente %u: chegou (%d/%d lugares ocupados)  \n", n, length(&queue), CADEIRAS);
			sem_post(&lock_c);
			sem_wait(&sEmpty);  // se tem gente sendo atendida espera, senão vai pro balcão
			sem_post(&full);  // avisa que tem gente no balcão
			sem_wait(&lock_cf[n]);  // espera o seu próprio atendimento ser realizado
		} else {
			printf("Cliente %u: cartorio lotado, saindo para dar uma volta (%d/%d lugares ocupados) \n", n, length(&queue), CADEIRAS);
			sem_post(&lock_c);
		}
		sleep(10);  // espera 10 s pra voltar pra fila
	}
} 
void *pAtendente(void *a) {
	unsigned int n= (unsigned	 int) a, c;
	while(1) {
		sem_wait(&full);  // se tem gente pra ser atendida continua, senão espera
		// exclusão mútua entre atendentes (dois atendentes não podem tentar chamar o primeiro da fila ao mesmo tempo)
		sem_wait(&lock_f);  
		c= dequeue(&queue);  // chama o cliente
		printf("Funcionario %u: atendendo cliente %u (%d/%d lugares ocupados) \n", n, c, length(&queue), CADEIRAS);
		sem_post(&lock_f); // a região crítica entre atendentes acabou
		sleep (5 + (rand () % 6));  // tempo de atendimento  
		sem_post(&lock_cf[c]);  // libera o cliente que foi sendo atendido
		printf("Funcionario %u: terminou de atender cliente %u (%d/%d lugares ocupados) \n", n, c, length(&queue), CADEIRAS);
		sem_post(&sEmpty);  // libera o espaço pro próximo cliente
	}
} 

int main() {
	init_queue (&queue);
	//printf("\x1b[32m");
	pthread_t  *lClien= malloc(sizeof(pthread_t) * TOTAL_CLIENTES),  // aloca as threads de clientes
				*lAtend= malloc(sizeof(pthread_t) * TOTAL_FUNCIONARIOS);  // aloca as threads de funcionarios

	sem_init(&sEmpty,0, TOTAL_FUNCIONARIOS);//semaforo dos clientes sendo atendidos
	sem_init(&full,0, 0);  // controle dos clientes sendo atendidos
	sem_init(&lock_f,0, 1);  //exclusão mútua entre os atendentes
	sem_init(&lock_c, 0, 1);  //exclusão mútua entre os clientes para entrar na fila
	lock_cf = malloc(sizeof(sem_t) * TOTAL_CLIENTES);  // aloca os semaforos dos clientes
	for (int i = 0; i< TOTAL_CLIENTES; i++)
		sem_init(&lock_cf[i],0, 0); // pra bloquear o cliente durante o atendimento
	
	/* cria os clientes */
	for (int i = 0; i < TOTAL_CLIENTES; i++)
		pthread_create(&lClien[i], NULL, pCliente, (void*) i); 
	/* cria os funcionários */
	for (int i = 0; i < TOTAL_FUNCIONARIOS; i++)  
		pthread_create(&lAtend[i], NULL, pAtendente, (void*) i);
	
	/* join Clientes */
	for (int i = 0; i < TOTAL_CLIENTES; i++)
		pthread_join(lClien[i], NULL); 
	/* join funcionários */
	for (int i = 0; i < TOTAL_FUNCIONARIOS; i++)
		pthread_join(lAtend[i], NULL);  
		
	/* DESTROI TUDO! */
	for (int i = 0; i< TOTAL_CLIENTES; i++)
		sem_destroy(&lock_cf[i]);
	sem_destroy(&lock_f);
	sem_destroy(&full);
	sem_destroy(&sEmpty);
	pthread_exit(NULL);
	return 0;
}
