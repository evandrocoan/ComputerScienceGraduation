#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdbool.h>
#include "queue.h"

#define CADEIRAS 4
#define TOTAL_FUNCIONARIOS 4
#define TOTAL_CLIENTES 10

sem_t sCliente, sFuncionario, sLivreBalcao, sBalcaoOcupado;
sem_t sCadaClie[TOTAL_CLIENTES]; // cada funcionario tera um semaforo proprio 
int i;
queue_t fila;
char pronto[TOTAL_CLIENTES];

void *func_clie(void *arg) {
	int idC = (int)arg;
	while (1) {
		
		sem_wait(&sCliente);
		if (length(&fila) < CADEIRAS){
					
			enqueue (&fila,idC);//coloca na fila
			printf("Cliente %d: chegou (%d/%d lugares ocupados)\n",idC,length(&fila),CADEIRAS);
			sem_post(&sCliente); // libera para o proximo cliente entrar na fila			
			
			sem_wait(&sLivreBalcao); // espera para ser atendido
			sem_post(&sBalcaoOcupado); // avisa que tem gente no balcao
			sem_wait(&sCadaClie[idC]); // poem em estado de bloqueado na fila para esperar ser atendido
			}	
		else{	
			printf("Cliente %d: cartorio lotado, saindo para dar uma volta(%d/%d lugares ocupados)\n",idC,length      				(&fila),CADEIRAS);
			sem_post(&sCliente);//libera verificação da fila
			
		}
		//printf("%d saiu \n", idC);
		sleep (10); // da um tempo de 10seg para voltar a fila de novo
	}
}	
void *func_func(void *arg) {
	int idF = (int)arg;
	while(1){
		sem_wait(&sBalcaoOcupado); // Bloqueia o funcionario para que se nao tem gente na fila, o mesmo nao tente atender
		sem_wait(&sFuncionario);//Apenas um funcionario retira o primeiro da fila				
		int id_cliente = dequeue(&fila);//retira o 1 da fila
		printf("Funcionario %d: atendendo cliente %d (%d/%d lugares ocupados)\n",idF,id_cliente,length(&fila),CADEIRAS);
		sem_post(&sFuncionario);//Libera o acesso ao primeiro da fila aos outros funcionarios		
		
		sleep(5+rand( )% 6);//Tempo de atendimento
		
		sem_post(&sCadaClie[id_cliente]); // libera o cliente apos ser atendido. Esse post eh do wait de quando o cliente esta 			esperando na fila
		printf("Funcionario %d: terminou de atender cliente %d (%d/%d lugares ocupados)\n",idF,id_cliente,length(&fila),CADEIRAS);
		sem_post(&sLivreBalcao);//Foi atendido
	}
}

int main(int argc, char **argv) {
	pthread_t clientes[TOTAL_CLIENTES], funcionarios[TOTAL_FUNCIONARIOS];
	sem_init(&sCliente, 0, 1); // cria semaforo de exclusao mutua para entrar na fila
	sem_init(&sFuncionario, 0, 1); // semaforo de exclusao mutua para tirar da fila
	sem_init(&sLivreBalcao,0,TOTAL_FUNCIONARIOS); // cria um semaforo para bloquear o cliente enquanto tem gente sendo atendida
	sem_init(&sBalcaoOcupado,0,0); // Cria um semaforo que bloqueia o funcionario quando nao tiver gente na fila
	for(int i=0; i<TOTAL_CLIENTES; i++) {
	  sem_init(&sCadaClie[i],0,0); // todo semaforo de clientes sempre estara em zero
	};

	init_queue (&fila);//inicia fila

	
	for(i=0; i<TOTAL_CLIENTES; i++) {
		pthread_create(&clientes[i], NULL, func_clie, (void*)i);//cria os clientes
	}
	for(i=0; i<TOTAL_FUNCIONARIOS; i++) {
		pthread_create(&funcionarios[i], NULL, func_func, (void*)i);//cria os funcionarios
	}
	for(i=0; i<TOTAL_CLIENTES; i++) {
		pthread_join(clientes[i], NULL);
	}
	for(i=0; i<TOTAL_FUNCIONARIOS; i++) {
		pthread_join(funcionarios[i], NULL);
	}
	sem_destroy(&sCliente);
	sem_destroy(&sFuncionario);
	
	pthread_exit(NULL);
}
