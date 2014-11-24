#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

int main(int argc, char **argv) {
    int status;
    pid_t pid = fork();
    
    printf("Novo processo!\n");
		
    return 0;
}
