#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void printmat( const char *s, double **m, int n );

int trianglize( double **m, int n );

double det( double *in, int n );

int transp( double **m, int n );

int soma( double **m, double **s, int nm, int ns );

int norma( double *m, int n );

int produto_escalar( double **m, int n, double esc );

int multiplica_escalar( double **m, int n, double esc );

int inversao( double **m, int n );

int produto_vetorial( double **m, int n, double esc );

int multiplica_matrizes( double **m, double **s, int nm, int ns );
