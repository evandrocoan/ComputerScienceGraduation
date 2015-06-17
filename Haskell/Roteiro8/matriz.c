#include "matriz.h"

void printmat( const char *s, double **m, int n )
{
    printf( "%s:\n", s );
    int i, j;
    
    for ( i = 0; i < n; i++ )
    {
        for ( j = 0; j < n; j++ )
        {
            printf( "%12.4f", m[ i ][ j ] );
        }
        putchar( '\n' );
    }
}

int trianglize( double **m, int n )
{
    int sign = 1;
    int i;
    
    for ( i = 0; i < n; i++ )
    {
        int max = 0;
        int row;
        
        for ( row = i; row < n; row++ )
        {
            if ( fabs( m[ row ][ i ] ) > fabs( m[ max ][ i ] ) )
            {
                max = row;
            }
        }
        
        if ( max )
        {
            sign = -sign;
            double *tmp = m[ i ];
            m[ i ] = m[ max ], m[ max ] = tmp;
        }
        
        if ( !m[ i ][ i ] )
        {
            return 0;
        }
        
        for ( row = i + 1; row < n; row++ )
        {
            double r = m[ row ][ i ] / m[ i ][ i ];
            if ( !r )
            {
                continue;
            }
            int col;
            
            for ( col = i; col < n; col++ )
            {
                m[ row ][ col ] -= m[ i ][ col ] * r;
            }
        }
    }
    return sign;
}

double det( double *in, int n )
{
    double *m[ n ];
    m[ 0 ] = in;
    int i;
    
    for ( i = 1; i < n; i++ )
    {
        m[ i ] = m[ i - 1 ] + n;
    }
    printmat( "Matrix", m, n );
    int sign = trianglize( m, n );
    
    if ( !sign )
    {
        return 0;
    }
    
    printmat( "Upper triangle", m, n );
    
    double p = 1;
    i = 0;
    
    for ( i = 0; i < n; i++ )
    {
        p *= m[ i ][ i ];
    }
    return p * sign;
}

