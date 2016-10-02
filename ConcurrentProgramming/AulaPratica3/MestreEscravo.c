#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main( int argc, char **argv )
{
    int size, rank, i;
    MPI_Init( &argc, &argv );
    char mensagem;
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    
    if( rank == 0 )
    {
        int rankRecebido = 0;
        mensagem = 'Olá, estou no RANK 0';
        for( i = 1; i < size; i++ )
            MPI_Send( &mensagem, 1, MPI_CHAR, i, 0, MPI_COMM_WORLD );
        for( i = 1; i < size; i++ )
        {
            MPI_Recv( &rankRecebido, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG,
                      MPI_COMM_WORLD, MPI_STATUS_IGNORE );
            printf( "rankRecebido : %d\n", rankRecebido );
        }
    }

    else
    {
        MPI_Recv( &mensagem, 1, MPI_CHAR, 0, MPI_ANY_TAG, MPI_COMM_WORLD,
                  MPI_STATUS_IGNORE );
        printf( "%c\n", mensagem );
        MPI_Send( &rank, 1, MPI_INT, 0, 0, MPI_COMM_WORLD );
    }
    MPI_Finalize( );
    return 0;
}

