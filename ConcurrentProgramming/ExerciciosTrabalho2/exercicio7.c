#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

#define MAX_THREADS 10

int var_compartilhada = 0;
sem_t sem;

void *func_thread(void *argumento) {
	for(int i=0; i < 100; i++) {
		sem_wait(&sem);
		var_compartilhada++;
		sem_post(&sem);
	}
	pthread_exit(NULL);
}

int main(int argc, char **argv) {
	pthread_t threads[MAX_THREADS];
	sem_init(&sem, 0,1);

	printf("Processo principal iniciado.\n");

	for(int i=0; i < MAX_THREADS; i++)
		pthread_create(&threads[i], NULL, func_thread, NULL);

	for(int i=0; i < MAX_THREADS; i++)
		pthread_join(threads[i], NULL);

	printf("Processo principal: var_compartilhada = %d.\n", var_compartilhada);

	sem_destroy(&sem);
	pthread_exit(NULL);
}
