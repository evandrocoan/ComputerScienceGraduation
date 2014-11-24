#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdbool.h>
#include "queue.h"
#define CADEIRAS 4 
#define TOTAL_FUNCIONARIOS 2
#define TOTAL_CLIENTES 10
int random;
sem_t sem_Cliente, sem_Funcionario, sem_EachClient[TOTAL_CLIENTES]; // cada funcionario tera um semaforo proprio para liberacao do funcionario, sem_Cliente e sem_Funcionario sao semaforos binarios para momentos críticos
queue_t fila; //fila de cadeiras (tamanho maximo que chega é o numero de cadeiras, se nao houver erro)

void *funcClient(void *arg) { //funcao das threads clientes
	int idClient = (int)arg; //Id da thread de cada cliente
	while (1) { //Loop infinito
		sem_wait(&sem_Cliente); //Semaforo Binario cada cliente checa de uma vez por causa do critico do enqueue fila, tem que ficar fora do if pois, se ficar dentro do if a condição pode mudar la dentro
		if (length(&fila) < CADEIRAS) {//Se tem lugar nas cadeiras
			enqueue (&fila,idClient) ; //adiciona o cliente a fila (as cadeiras)
			printf("Cliente %d: chegou no cartorio \033[1m(%d/%d lugares ocupados)\033[m\n",idClient,length(&fila),CADEIRAS);
			sem_post(&sem_Cliente); //Libera o semaforo binario para o proximo cliente checar
			sem_wait(&sem_EachClient[idClient]); // poem em estado de bloqueado na fila para esperar terminar de ser atendido
		}
		else {
			printf("Cliente %d: cartorio lotado, cliente saindo para dar uma volta \033[1m(%d/%d lugares ocupados)\033[m\n",idClient,length      				(&fila),CADEIRAS);
			sem_post(&sem_Cliente); //Libera o semaforo binario para o proximo cliente checar
		}
		printf("\033[31mCliente %d saiu do cartorio\033[m\n",idClient);
		sleep (10); // da um tempo de 10seg para voltar ao cartorio de novo
	}
}
void *funcFuncionario(void *arg) {
	int idFuncionario = (int)arg; //pega qual funcionario e
	while(1) { //loop infinito
		sem_wait(&sem_Funcionario); //Semafaro binario pra nao dar problema na fila, tem que ser antes do if pois se nao a condição pode mudar esperando dentro do if
		if(length(&fila) > 0) { //se tiver gente na fila
		int id_cliente = dequeue(&fila); //tira o cliente da fila de cadeiras
		printf("\033[34mFuncionario %d: atendendo cliente %d \033[1m(%d/%d lugares ocupados)\033[m\n",idFuncionario,id_cliente,length(&fila),CADEIRAS);
		
		random = rand( )% 6;
		random += 5;
		sem_post(&sem_Funcionario); //Libera o semaforo binario de funcionario para nao dar problema na fila
		sleep(random); //dorme de 5 a 10 segundos
		printf("\033[34mFuncionario %d: terminou de atender cliente %d em %d segundos \033[1m(%d/%d lugares ocupados)\033[m\n",idFuncionario,id_cliente,random,length(&fila),CADEIRAS);
		sem_post(&sem_EachClient[id_cliente]); //libera o cliente apos ser atendido, bota o semaforo em 1 que logo vai ser mudado pra zero de novo pelo cliente esperando
		} else { 
			printf("\033[34mFuncionario %d: Sem clientes na fila, darei um coffee break \033[1m(%d/%d lugares ocupados)\033[m\n",idFuncionario,length(&fila),CADEIRAS);
                sem_post(&sem_Funcionario);
		sleep(10);
		}
	}
}
int main(int argc, char **argv) {
	pthread_t clientes[TOTAL_CLIENTES], funcionarios[TOTAL_FUNCIONARIOS]; //cria arrays com clientes e funcionarios
	sem_init(&sem_Cliente, 0, 1);
	sem_init(&sem_Funcionario, 0, 1);
	int i;
	for(i=0; i<TOTAL_CLIENTES; i++) {
	  sem_init(&sem_EachClient[i],0,0); // Começa em zero, assim primeiramente o cliente ira esperar wait (ficar 1) e quando for atendido ao final do atendimento o funcionario botará em 1, e o cliente em 0 novamente!
	};
	init_queue (&fila);
	for(i=0; i<TOTAL_CLIENTES; i++) {
		pthread_create(&clientes[i], NULL, funcClient, (void*)i);//cria os clientes
	}
	for(i=0; i<TOTAL_FUNCIONARIOS; i++) {
		pthread_create(&funcionarios[i], NULL, funcFuncionario, (void*)i);//cria os funcionarios
	}
	for(i=0; i<TOTAL_CLIENTES; i++) {
		pthread_join(clientes[i], NULL);
	}
	for(i=0; i<TOTAL_FUNCIONARIOS; i++) {
		pthread_join(funcionarios[i], NULL);
	}

//destroy semaphores and exit
	sem_destroy(&sem_Cliente);
	sem_destroy(&sem_Funcionario);
	for(i=0; i<TOTAL_CLIENTES; i++) sem_destroy(&sem_EachClient[i]);
	pthread_exit(NULL);
}
