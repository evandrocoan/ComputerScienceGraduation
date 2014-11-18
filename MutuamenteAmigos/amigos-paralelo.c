#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

int main(int argc, char **argv) {

	int minimo = atoi(argv[1]);
	int maximo = atoi(argv[2]);
	int numThreads = atoi(argv[3]);
	//printf("minimo: %d", minimo);
	//printf("maximo: %d", maximo);
	int range = maximo - minimo;
	int i, j;
	double amigos[range];
	double fracaoA;

	omp_set_num_threads(numThreads);
		for (i = minimo; i <= maximo; i++) {
			fracaoA = (double) somaDivisores(i) / i;
			amigos[i - minimo] = fracaoA;
		}

		for(j = 0; j <= range; j++){

		#pragma omp parallel for schedule(static)
			for (i = j+1; i <= range; i++) {
		    	if(amigos[j] == amigos[i]) {
	        	   printf("Os numeros %d e %d são mutuamente amigos.\n",  (minimo +i), (minimo+j));
		    	}
			}

	}
	return 0;
}

int somaDivisores(int valor) {
	int extra = (int) sqrt(valor),j, divid, soma = 0;
				for(j = 1; j <= extra; j++){
					if(valor % j == 0) {
						divid = valor/j;
					//	printf("\nDivisores de %d: %d",valor, j);
						//printf("\nDivisores de %d: %d",valor, valor/j);
						if((valor/j) == j)
							divid = 0;
						soma += j + divid;

					}
				}
				return soma;
	}



