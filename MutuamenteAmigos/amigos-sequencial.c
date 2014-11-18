#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

int main(int argc, char **argv) {

	//valores maximos e minimos fornecidos pelo usuario
	int minimo = atoi(argv[1]);
	int maximo = atoi(argv[2]);
	//printf("minimo: %d", minimo);
	//printf("maximo: %d", maximo);

	//tamanho do intervalo
	int range = maximo - minimo;
	int i, j;
	//array para armazenar o resultado da razão da soma dos divisores de um numero pelo numero
	double amigos[range];
	//variavel para auxiliar o calculo
	double fracaoA;

	//este for calcula a razao da soma dos divisores de todos os numeros do intervalo pelo proprio numero e armazena este numero no array amigos
		for (i = minimo; i <= maximo; i++) {
			fracaoA = (double) somaDivisores(i)/i;
			amigos[i - minimo] = fracaoA;
		}
	//este for percorre o array e compara para ver se os resultados são iguais, caso sim, são mutuamente amigos.
		for(j = 0; j <= range; j++){
			for (i = j+1; i <= range; i++) {
		    	if(amigos[j] == amigos[i]) {
	        	   printf("Os numeros %d e %d são mutuamente amigos.\n",  (minimo +i), (minimo+j));
		    	}
		}
	}
	return 0;
}

//metodo que soma os divisores de um numero. Utiliza uma propriedade matematica para diminuir a complexidade do algoritmo.
int somaDivisores(int valor) {
	int extra = (int) sqrt(valor),j, divid, soma = 0;;
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



