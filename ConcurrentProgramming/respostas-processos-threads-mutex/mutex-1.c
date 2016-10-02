#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#define MAX_THREADS 10

int var_compartilhada = 0;
pthread_mutex_t lock;

void *func_thread(void *argumento) {
	for(int i=0; i < 100; i++) {
		pthread_mutex_lock(&lock);
		var_compartilhada++;
		pthread_mutex_unlock(&lock);
	}
	pthread_exit(NULL);
}

int main(int argc, char **argv) {
	pthread_t threads[MAX_THREADS];
	pthread_mutex_init(&lock, NULL);

	printf("Processo principal iniciado.\n");

	for(int i=0; i < MAX_THREADS; i++)
		pthread_create(&threads[i], NULL, func_thread, NULL);

	for(int i=0; i < MAX_THREADS; i++)
		pthread_join(threads[i], NULL);

	printf("Processo principal: var_compartilhada = %d.\n", var_compartilhada);

	pthread_mutex_destroy(&lock);
	pthread_exit(NULL);
}