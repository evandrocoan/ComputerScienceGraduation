clc
clear
format long
A=[3 -1 -1  1; 
   1  3  1  5;
   2 -2  4  4;]
n=3;%ou n=size(A,1)
tol=1e-6
xi=[0 0 0]
% "Metodo de Jacobi"
% x=fjacobi(n,A,xi,tol)
"Metodo de Gauss Seidel"
lambda=0.85
x=fgaussseidel(n,A,xi,tol,lambda)