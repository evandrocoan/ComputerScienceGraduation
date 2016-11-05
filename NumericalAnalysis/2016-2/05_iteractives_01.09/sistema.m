clc
format long

%Declaração da Matriz
A=[3 -1 -1 1; 1 3 1 5; 2 -2 4 4;]
n= size(A,1);
xi=[0 0 0];
tol=1e-8

printf("\n\nMÉTODO DE JACOBI\n")
x = fjacobi(n,A,xi,tol)
xe=fjacobi(n,A,x,tol^2);
printf("\nERRO MÁXIMO DA SOLUÇÃO POR JACOBI\n")
errojacobi=max(abs(x .- xe))

printf("\n\nMÉTODO DE GAUSS-SEIDEL\n")
[x cont k diferenca] = fgaussseidel(n,A,xi,tol)
[xe cont k diferenca] =fgaussseidel(n,A,x,tol^2);
printf("\nERRO MÁXIMO DA SOLUÇÃO POR GAUSS-SEIDEL\n")
errogaussseidel=max(abs(x .- xe))