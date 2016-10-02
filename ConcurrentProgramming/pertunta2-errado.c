#include<omp.h>
#include<stdio.h>

int tid;

int main() {
    #pragma omp parallel
    {
        tid = omp_get_thread_num();
        printf("Hello from thread %d \n", tid);
    }
}