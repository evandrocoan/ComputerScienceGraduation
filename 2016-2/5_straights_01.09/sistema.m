format long

A = [   sqrt(2)      e           sqrt(2)       1;
        sqrt(3)      sqrt(10)   -1/sqrt(3)     2;
        sqrt(3)      sqrt(10)   1/sqrt(3)     3;]
n = size(A,1)
x = fgauss(n,A)
rmax=fresiduo(n,A,x)