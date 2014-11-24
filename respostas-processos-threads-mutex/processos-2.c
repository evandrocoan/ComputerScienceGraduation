#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

int main(int argc, char **argv) {
    int status, i;
    pid_t pid;

    for(i = 0; i < 4; i++) {
     	pid = fork();
     	if (pid == 0) { //filho
     		printf("Processo filho %u\n", getpid());
     		break;
     	}
     	else { //pai
     		printf("Processo pai %u criou %u\n", getpid(), pid);
     	}
    }
		
    return 0;
}
