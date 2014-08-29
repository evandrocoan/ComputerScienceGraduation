/**
 * Produto escalar concorrente usando threads
 *
 * \author Luiz Henrique, Evandro  Coan
 * \version 1.0
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NTHREADS 4
#define VECSIZE 16

pthread_t threads[NTHREADS];//threads que faram o calculo
pthread_mutex_t mutex;//mutex para exclusao mutua
int a[VECSIZE], b[VECSIZE];
int sum, tamanho;//variaveis para auxiliar a soma

/**
 * Faz o produto escalar da thread.
 *
 * \param índice do arranjo da thread. Serve para saber onde a thread começa a fazer o cálculo
 */
void *produtoEscalar(void *arg) { //funcao para calcular o produto escalar

	// inicialização de variaveis
	int i, inicio, fim;
	long numThread;
	int produtoEscalarParcial, *x, *y;
	numThread = (long) arg;
	//pthread_mutex_lock(&mutex);

    //------------ lançar um exeção para NTHREADS & VECTOR SIZE == 0, EVANDRO

	// numero de posições que cada thread calculará
	int quant = VECSIZE / NTHREADS;

	// separa entre um numero de vetores compativel com o número de threads
	// ou seja, enter o caso de haver divisão inteira entre o número de thread e o número de posiçõe no vetor
	if(quant%1==0){
	// retira-se 1 de inicio para a proxima thread não pegar o final da anterior
	inicio = numThread * quant - 1; //thread pega limite baseada no seu numero
	fim = inicio + quant;
	x = a;
	y = b;

	inicio++; // incrementa-se para não iniciar o arranjo em -1

	} else { // calcula quando a não há uma divisão inteira

		int quant_resto = VECSIZE % NTHREADS;

		// separa a última thread das demais
		if( NTHREADS == numThread )
		{
			numThread = numThread - 1; //

			inicio = numThread * quant - 1; //thread pega limite baseada no seu numero

		} else{
			numThread = numThread - 1; //

			inicio = numThread * quant - 1; //thread pega limite baseada no seu numero

			fim = inicio + quant;
			x = a;
			y = b;

			inicio++; // ajusta a posição correta

		}

	}

	produtoEscalarParcial = 0;
	for (i = inicio; i <= fim; i++) {//calcula o produto escalar parcial de cada thread
		produtoEscalarParcial += (x[i] * y[i]);
	}

	// impede que as thread alterem o valor da soma ao mesmo tempo
	pthread_mutex_lock(&mutex); //da lock para uma thread de cada vez fazer a soma
	sum += produtoEscalarParcial;
	printf("\nThread %ld calculou de %d a %d: produto escalar parcial = %d",
			numThread, inicio, fim, produtoEscalarParcial);
	pthread_mutex_unlock(&mutex);// libera o lock

	// mata a thread quando ela termina o trabalho
	pthread_exit((void*) 0);

}

/**
 *
 *
 */
int main(int argc, char *argv[]) {
	srand(time(NULL));
	long i;
	int cont = 0;

	for (i = 0; i < VECSIZE; i++) {//gera o vetor random
		a[i] = rand() % 10;
		b[i] = rand() % 10;
	}

	pthread_mutex_init(&mutex, NULL);//inicializa o mutex

	printf("A = ");
	while (cont < VECSIZE) {//imprime o vetor A
		printf("%d, ", a[cont]);
		cont++;
	}
	cont = 0;
	printf("\nB = ");
	while (cont < VECSIZE) {//imprime o vetor B
		printf("%d, ", b[cont]);
		cont++;
	}
	for (i = 0; i < NTHREADS; i++) {//cria as threads
		pthread_create(&threads[i], NULL, produtoEscalar, (void *) i);
		pthread_join(threads[i], NULL);
	}/*
	for (i = 0; i < NTHREADS; i++) {//aguarda a execução das threads filhas
		pthread_join(threads[i], NULL);
	}*/
	printf("\nProduto escalar =  %d \n", sum);//imprime o resultado final
	if(sum!=3974)
		printf("=====================================================================================");
	pthread_mutex_destroy(&mutex);//destrói o mutex
	pthread_exit(NULL);
}
