#include <stdio.h>
#include <unistd.h>
#include <pthread.h>


void *func_thread(void *argumento) {
	pthread_t tid = pthread_self();
	printf("Thread %u iniciada.\n", (unsigned int) tid);
	pthread_exit(NULL);
}

int main(int argc, char **argv) {

	pthread_t thread;

	printf("Processo principal iniciado.\n");

	pthread_create(&thread, NULL, func_thread, NULL);
	pthread_join(thread, NULL);
	pthread_exit(NULL);
}