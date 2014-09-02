/**
 * Produto escalar concorrente usando threads
 *
 * \author Luiz Henrique, Evandro  Coan
 * \version 1.0
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NTHREADS 100
#define VECSIZE 100

//threads que faram o calculo
pthread_t threads[NTHREADS];

//mutex para exclusao mutua
pthread_mutex_t mutex;

//vetores para o cálculo
int a[VECSIZE], b[VECSIZE];

//variaveis para auxiliar a soma
int sum, tamanho;

/**
 * Imprime os vetores os atributos 'A' e 'B'  
 */
void printArray()
{
   int cont =0;
   printf("\nA = ");

   //imprime o vetor A
   while (cont < VECSIZE) 
   {
       printf("%d, ", a[cont]);
       cont++;        
   }
   cont = 0;
   printf("\nB = ");
   while (cont < VECSIZE) 

   //imprime o vetor B
   {
       printf("%d, ", b[cont]);
       cont++;    
   }
}

/**
 * Realiza o produto escalar 
 * 
 * @param arg o numero da thread que está executando
 */
void *produtoEscalar(void *arg) 
{
   int i, inicio, fim;
   long numThread;
   int produtoEscalarParcial = 0, *x, *y;
   numThread = (long) arg;
   int quant = VECSIZE / NTHREADS;

   //thread pega limite baseada no seu numero
   inicio = numThread * quant - 1; 
   fim = inicio + quant;
   x = a;
   y = b;

   inicio++;

   //calcula o produto escalar parcial de cada thread
   for (i = inicio; i <= fim; i++) 
   {
     produtoEscalarParcial += (x[i] * y[i]);
   }

   //da lock para uma thread de cada vez fazer a soma
   pthread_mutex_lock(&mutex); 
   
   //realiza a soma e imprime
   sum += produtoEscalarParcial;
   printf("\nThread %ld calculou de %d a %d: produto escalar parcial = %d",
           numThread, inicio, fim, produtoEscalarParcial);
   
   //libera o lock
   pthread_mutex_unlock(&mutex);
   pthread_exit((void*) 0);
}
/**
 * 
 * @param argc não tem funcionalidade neste programa
 * @param argv não tem funcionalidade neste programa
 * @return se o programa foi executado corretamente chegando ao final de sua 
 * execução
 */
int main(int argc, char *argv[]) {
   srand(time(NULL));
   long i;
   if(VECSIZE%NTHREADS!=0 || VECSIZE < NTHREADS)
   {
      printf("\nDivisão não inteira");
      exit(0);
   }
   //gera o vetor random
   for (i = 0; i < VECSIZE; i++) 
   {
      a[i] = rand() % 10;
      b[i] = rand() % 10;
   }

   //imprime os vetores
   printArray();

   //inicializa o mutex
   pthread_mutex_init(&mutex, NULL);

   //cria as threads
   for (i = 0; i < NTHREADS; i++) 
   {
      pthread_create(&threads[i], NULL, produtoEscalar, (void *) i);
   }

   //aguarda a execução das threads
   for (i = 0; i < NTHREADS; i++) {
      pthread_join(threads[i], NULL);
   }

   //imprime o resultado final
   printf("\nProduto escalar =  %d \n", sum);

   //destrói o mutex
   pthread_mutex_destroy(&mutex);

   //fecha a thread
   pthread_exit(NULL);
}
