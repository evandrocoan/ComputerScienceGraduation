
n = 10; 
i = 1;                           A(1, 1) = 1; A(1, 2)   = 1; A(1, n+1) = 3;
for i=2 : n-1     A(i, i-1) = 1; A(i, i) = 2; A(i, i+1) = 1; A(i, n+1) = 10; end
i = n;            A(i, i-1) = 1; A(i, i) = 1;                A(i, n+1) = 2;
A %sistema sem convergência garantida porque não tem diagonal dominante

%x = fgauss(n, A); %opera todos os coeficientes nulos, sem necessidade

%x = fGaussSeidel(n, A) não convergiu, nem com fator de sub-relaxação

%algoritmo de escalonamento de Gauss otimizado para matrizes tridiagonais
%armazenamento das diagonais através de vetores

r(1) = 1; d(1) = 1; b(1) = 1;

for i=2 : n-1
    t(i) = 1; r(i) = 2; d(i) = 1; b(i) = 10;
end

t(n) = 1; r(n) = 1; b(n) = 2;

t
r
d
b

