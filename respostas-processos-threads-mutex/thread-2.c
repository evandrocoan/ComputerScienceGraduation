#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#define MAX_THREADS 2

void *func_thread(void *argumento) {
	pthread_t tid = pthread_self();
	printf("Thread %u iniciada.\n", (unsigned int) tid);
	pthread_exit(NULL);
}

int main(int argc, char **argv) {

	pthread_t threads[MAX_THREADS];

	printf("Processo principal iniciado.\n");

	for(int i=0; i < MAX_THREADS; i++)
		pthread_create(&threads[i], NULL, func_thread, NULL);

	for(int i=0; i < MAX_THREADS; i++)
		pthread_join(threads[i], NULL);

	pthread_exit(NULL);
}