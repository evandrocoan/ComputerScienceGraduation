#include <omp.h>
#include <stdio.h>

int tid;
int num_threads = 4;
int main() 
{
	omp_set_num_threads(num_threads);
#pragma omp parallel firstprivate(tid)
	{
		tid = omp_get_thread_num();
		printf("Hello from thread %d \n", tid);
	}
}