#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int main( int argc, char **argv )
{
	#pragma omp parallel
	{
		printf("Thread %d: iniciada\n", omp_get_thread_num() );
		#pragma omp single
		printf("Total de threads = %d\n", omp_get_num_threads() );
		
		printf("Thread %d: iniciada\n", omp_get_thread_num() );
	}
	{
		#pragma omp sections
		{
			#pragma omp section
			{
				printf("Seção1\n");
				sleep(3);
				printf("Seção1.1\n");
			}
			#pragma omp section
			{
				printf("Seção2\n");
				sleep(3);
				printf("Seção2.1\n");
			}
			#pragma omp section 
			{
				printf("Seção3\n");
			}
		}
		sleep(3);
		printf("Seção4\n");
		
		int x = 0;
		#pragma omp parallel shared(x)
		{
			#pragma omp critical 
			x++;
			
			printf("x dentro do laço = %d\n", x);
		}
		printf("x fora do laço = %d\n", x);
	}
}