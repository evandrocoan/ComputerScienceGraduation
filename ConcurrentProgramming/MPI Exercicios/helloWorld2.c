#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main( int argc, char **argv )
{
	MPI_Init(&argc, &argv);
	
	int numbersOfProcess, myRank;
	MPI_Comm_size	(MPI_COMM_WORLD, &numbersOfProcess);
	MPI_Comm_rank(MPI_COMM_WORLD, &myRank );
	
	printf( "Hello there are %d process!\n", numbersOfProcess );
	
	
	MPI_Finalize();
	
	return 0;
}