#include <stdio.h>
#include <omp.h>
#define tamanho 10

int main(int argc, char **argv){
int A[tamanho] ={1,1,1,1,1,1,1,1,1,1};
int B[tamanho] ={1,1,1,1,1,1,1,1,1,1};

int i = 0;
int soma = 0;


#pragma omp parallel for reduction(+:soma)
for(i=0;i<tamanho;i++)
soma += A[i]*B[i];

printf("soma %d\n",soma);
return 0;
}
