#include <math.h>       /* sqrt */
#include <stdio.h>
#include <omp.h>
#include <stdlib.h>

int calcula(int numero);

int main(int argc, char **argv) {

int min = atoi(argv[1]);
int max = atoi(argv[2]);
int nT = atoi(argv[3]);
/*int min, max;
printf("\nDigite o número mínimo: ");
scanf("%d", &min);
printf("\nDigite o número máximo: ");
scanf("%d", &max);
*/
omp_set_num_threads(nT);
//double *aux = (double *) malloc(sizeof(double) * (max-min)+1);
int tam = max-min;
double aux[(tam)+1];
int i, j;
#pragma omp parallel for schedule(dynamic, 5) shared(aux)
for (i = 0; i <= tam; i++)
	aux[i] = ((double)calcula(min+i))/(min+i);
#pragma omp parallel for private (j) schedule(dynamic, 5)
for (i = 0; i < tam; i++)
	for (int j = i+1; j <= tam; j++)
		if (aux[i]==aux[j]);
 	          //printf("Os números %d e %d são amigos.\n", min+i, min+j);
//  free(aux);

}
/**
 * Calcula soma dos divisores
 * @param numero a ser calculado os divisores
 * @return soma dos divisores30
 */

int calcula(int numero) {
int acc = 1 + numero;
int lim = numero/2;
#pragma omp paralel for schedule(static) reduction(+: acc) shared(acc, lim)
for (int i = 2; i <= lim; i++) {
	if (numero % i == 0) {
		acc += i;
	}
}
 return acc;
}
