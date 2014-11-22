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

int calcula(int valor) {
	// obtém o limite do for, que é a raíz quadrada do número
	int extra = (int) sqrt( valor );
	
	// declara a variável para que servirá de indice para o for;
	int j;
	
	// declara a variável que armazena o quociente, que, matematicamente, 
	// também será um divisor do número 
	int divid;
	
	// declara a variável que armazenará a soma dos dos divisores, e inicializa com 
	// zero porque na primeira interação ela será preenchida carregado com os 
	// divisores padrões de um número, 1 e ele próprio
	int soma = 0;
	
	// define o inicio da execução paralela
	//#pragma omp paralel for schedule(guided) reduction(+:soma) private(divid)
	// interage até a raiz do número
	for( j = 1; j <= extra; j++ )
	{
		//verifica-se se há um divisor do número
		if( valor % j == 0 )
		{
			// armazena o quociente do divisor do número
			divid = valor / j;
			
			// retira o quociente da soma quando se chega no último número da iteração
			if( ( valor / j ) == j )
			divid = 0;
			
			// utiliza a propriedade matemática que diz que os divisores e o quociente
			// quando cálculados até a raíz de um número são representam seus dividores
			soma += j + divid;
		}
	}
	return soma;
}
