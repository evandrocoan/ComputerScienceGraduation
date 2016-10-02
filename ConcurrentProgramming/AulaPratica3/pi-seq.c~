#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

unsigned int compute_pi(unsigned int, unsigned int);

int main(int argc, char **argv){
  unsigned int pontos_no_circulo,rank,pontos,qtdeProcessos;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &qtdeProcessos);

  pontos = atoi(argv[1]);

  if(argc != 2 || qtdeProcessos > pontos){
    printf("Uso:\n");
    printf("\t%s <numero de pontos a serem sorteados>\n", argv[0]);
    MPI_Finalize();
    return 0;
  }

if(rank == 0){
  unsigned int i,passou,pontosCalcular,temp;

  ////dividir a qtde de pontos entre os processos
  if(pontos%(qtdeProcessos-1)==0){
  passou = 0;
  pontosCalcular = pontos/(qtdeProcessos-1);
  }else{
  passou = pontos%(qtdeProcessos-1);
  pontosCalcular = pontos-passou;
  pontosCalcular = pontosCalcular/(qtdeProcessos-1);  
  }

  ////enviar para cada processo a qtd
  for(i =1;i<qtdeProcessos-1;i++)
  MPI_Send(&pontosCalcular, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
  pontosCalcular = pontosCalcular+passou;
  MPI_Send(&pontosCalcular, 1, MPI_INT, qtdeProcessos-1, 0, MPI_COMM_WORLD);

  ////esperar todas respostas
  pontos_no_circulo = 0;
  temp = 0;
  for(i=1;i<qtdeProcessos-1;i++){
  MPI_Recv(&temp, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  pontos_no_circulo = pontos_no_circulo+temp;
  }

  // calcula a aproximacao de Pi baseado nos pontos sorteados
  printf("Pi = %.040f\n", ((double)pontos_no_circulo/(double)pontos)*4);


}else{
  ////aguardar saber qnts pontos deve alcular
  MPI_Recv(&pontos, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  // retorna quantos pontos sorteados cairam dentro do circulo
  pontos_no_circulo = compute_pi(rank, pontos);
  ////responder calculo
  MPI_Send(&pontos_no_circulo, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
}
  MPI_Finalize();
  return 0;
}

unsigned int compute_pi(unsigned int seed, unsigned int pontos){
  unsigned int i;
  unsigned int pontos_no_circulo;
  double x, y;

  pontos_no_circulo = 0;
  srand(seed);

  for(i=0; i<pontos; i++){
  // sorteia um ponto: coordenadas x e y dentro do quadrado
    x = (double)rand()/(double)(RAND_MAX);
    y = (double)rand()/(double)(RAND_MAX);      
    
    // verifica se o ponto sorteado encontra-se dentro do circulo
    if( (x*x + y*y) < 1 ){
      pontos_no_circulo++;
    }      
  }
  
  return pontos_no_circulo;
}
//random
