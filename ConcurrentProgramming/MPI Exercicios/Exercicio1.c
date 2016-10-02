#include <stdio.h>
#include <mpi.h>

int main( int argc, char** argv )
{
    int rank, size, dado = 0;
    MPI_Status st;
    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    int i;
    if( rank == 0 )
    {
        for( i = 0; i < size; i++ )
            MPI_Send( &dado, 1, MPI_INT, i, 0, MPI_COMM_WORLD );
        for( i = 0; i < size; i++ )
        {
            MPI_Recv( &rank, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG,
                      MPI_COMM_WORLD, &st );
            printf( "\nRank : %d\n", rank );
        }
    } else
    {
        MPI_Recv( &dado, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &st );
        //printf("\nDado: %d",dado);
        MPI_Send( &rank, 1, MPI_INT, 0, 0, MPI_COMM_WORLD );
    }
    
    MPI_Finalize( );
    return 0;
}
