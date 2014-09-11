% Script principal
clear
clc
format long

% Matriz composta
Ac = [
	[2.5	1.0	1.5	0.0	0.0 	0.0	0.0	0.0	0.0	0.0	4];
    	[0.0	0.52	0.51	0.0 	0.1 	0.0	0.0	0.0   0.0   0.0  -3];
    	[0.9	1.0	2.9	1.0   0.0   0.0   0.0   0.0   0.0   0.0   1];
    	[0.0	1.0	0.2	2.2   1.0   0.0   0.0   0.0   0.0   0.0  -1];
    	[1.0	0.0	0.0	2.0  	4.0  	1.0   0.0   0.0   0.0   0.0  -1];
    	[0.0	1.0	0.0	0.0  -2.0  	4.0  -1.0   0.0   0.0   0.0   0];
    	[1.0	0.0	0.0	0.0   0.0   2.0   4.0   1.0   0.0   0.0  -1];
    	[0.0	1.0	0.0	0.0   0.0   0.0   1.0   3.0   1.0   0.0   1];
    	[0.0	0.0	1.0	0.0   0.0   0.0   0.0  -1.0  -3.0  -1.0   3];
    	[0.0	0.0	0.0  	1.0   0.0	0.0   0.0   0.0   1.0   2.0  -2];
    ];

A = [
	[2.5	1.0	1.5	0.0	0.0 	0.0	0.0	0.0	0.0	0.0];
    	[0.0	0.52	0.51	0.0 	0.1 	0.0	0.0	0.0   0.0   0.0];
    	[0.9	1.0	2.9	1.0   0.0   0.0   0.0   0.0   0.0   0.0];
    	[0.0	1.0	0.2	2.2   1.0   0.0   0.0   0.0   0.0   0.0];
    	[1.0	0.0	0.0	2.0  	4.0  	1.0   0.0   0.0   0.0   0.0];
    	[0.0	1.0	0.0	0.0  -2.0  	4.0  -1.0   0.0   0.0   0.0];
    	[1.0	0.0	0.0	0.0   0.0   2.0   4.0   1.0   0.0   0.0];
    	[0.0	1.0	0.0	0.0   0.0   0.0   1.0   3.0   1.0   0.0];
    	[0.0	0.0	1.0	0.0   0.0   0.0   0.0  -1.0  -3.0  -1.0];
    	[0.0	0.0	0.0  	1.0   0.0	0.0   0.0   0.0   1.0   2.0];
    ];

B = [
	 4;
	-3;
	 1;
	-1;
	-1;
	 0;
	-1;
	 1;
	 3;
	-2;
    ];

% Verificar se o sistema é mal-condicionado:
	c = condicionamento(A);
	if(c)
		printf("O sistema NÃO é mal condicionado\n");
	else
		printf("O sistema é mal condicionado\n");
	endif 

% Verificar convergência garantida:
	c = convergencia(A);
	if(c)
		printf("A tem convergencia garantida\n");
	else
		printf("A NAO tem convergência garantida\nRecomendado usar fator de sub-relaxamento\n");
	endif

printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
% Gauß:
	nAc = rows( Ac );
	ACaux = Ac;
	% Escalonamento de Gauß nº1:
	printf("Resultado pelo Escalonamento de Gauß\n");
	tic();
	x = fGauss (nAc, Ac)
	tempo=toc();
	printf("Resíduo pelo Escalonamento de Gauß\n");
	res = fResiduo(nAc, ACaux, x)
	printf("Tempo de Processamento do Escalonamento de Gauß\n");
	tempo
	% Escalonamento de Gauß nº2 - Contando o número de operações com ponto flutuante:
	printf("Número de Operações com Ponto Flutuante em Escalonamento de Gauß\n");
	op = fGauss2 (nAc, Ac)

printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
% LU Crout:
	op = 0;
	n = rows( A );
	Aaux = [ A B ];
	% Escalonamento LU de Crout nº1:
	printf("Resultado pelo Escalonamento LU de Crout\n");
	tic();
	[A B] = fLUCrout(n, A, B);
	x = fSub1( n, A, B )
	tempo=toc();
	printf("Resíduo pelo Escalonamento LU de Crout\n");
	res = fResiduoCrout(n, Aaux, x)
	printf("Tempo de Processamento do Escalonamento LU de Crout\n");
	tempo
	% Escalonamento LU de Crout nº2 - Contando o número de operações com ponto flutuante:
	[A B o1] = fLUCrout2(n, Aaux, B);
	[x o2] = fSub2( n, A, B );
	printf("Número de Operações com Ponto Flutuante em Escalonamento LU de Crout\n");
	op = o1 + o2
	
printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
% Jacob:
	errMax = 1e-6;
	numIt = 1000;
	fat = 0.9;
	printf("Resultado pelo Escalonamento Iterativo de Jacob\n");
	tic();
	x = jacob(errMax, numIt, fat)
	tempo=toc();
	printf("Tempo de Processamento do Escalonamento Iterativo de Jacob\n");
	tempo
	printf("Número de Operações com Ponto Fluante em Escalonamento de Jacob\n");
	op = jacob2(errMax, numIt, fat)

printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
% Gauß-Seidel
	fat = 1; %Sem subrelaxação
	printf("Resultado pelo Escalonamento Iterativo de Gauß-Seidel\n");
	tic();
	x = gSeidel(errMax, numIt, fat)
	tempo=toc();
	printf("Tempo de Processamento do Escalonamento Iterativo de Gauß-Seidel\n");
	tempo
	printf("Número de Operações com Ponto Fluante em Escalonamento de Gauß-Seidel\n");
	op = gSeidel2(errMax, numIt, fat)
