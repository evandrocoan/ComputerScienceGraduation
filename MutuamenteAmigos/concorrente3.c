#include <stdio.h>

int main(int argc, char **argv) {
int min = atoi(argv[1]);
int max = atoi(argv[2]);
/*int min, max;
printf("\nDigite o número mínimo: ");
scanf("%d", &min);
printf("\nDigite o número máximo: ");
scanf("%d", &max);
*/

double aux[(max-min)+1];
for (int i = 0; i <= (max - min); i++)
	aux[i] = ((double)calcula(min+i))/(min+i);
for (int i = 0; i < (max-min); i++)
	for (int j = i+1; j <= (max-min); j++)
		if (aux[i]==aux[j]);
 	          //printf("Os números %d e %d são amigos.\n", min+i, min+j);
}

/**
 * Calcula soma dos divisores
 * @param numero a ser calculado os divisores
 * @return soma dos divisores30
 */

int calcula(int numero) {
int acc = 1 + numero;
for (int i = 2; i <= numero/2; i++) {
	if (numero % i == 0) {		
		acc += i;
	}
}       
 return acc;
}
