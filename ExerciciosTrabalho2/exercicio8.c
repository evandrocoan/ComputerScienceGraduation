#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

sem_t cheio = 0;
sem_t vazio = 10;
sem_t lock_prod = 1;
sem_t lock_cons = 1;
int buffer[10];

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



int main(int argc, char **argv) {
	
	pthread_t threadsConsumidoras[MAX_THREADS];
	pthread_t threadsProdutoras[MAX_THREADS];
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
