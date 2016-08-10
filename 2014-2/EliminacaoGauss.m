% método de escalonamento de Gauss
n = 3
A = [-1.1 2 3 4; 5 6.2 7 8; 9 0.5 10 2; ]
Aaux = A;
% Complexidade O(n³)
for k = 1: n - 1
  for i = k + 1: n
    aux = A(i,k) / A(k,k)
    A(i,k) = 0;
    for j = k + 1: n+1
      % Li <= Li - x*Lk
      A(i,j) = A(i,j) - aux * A(k,j); 
    end
  end
end
A

% Resolução das variáveis
x(n) = A(n,n + 1) / A(n,n);
% "-1" significa decremento
for i = n - 1: -1: 1 
  aux = 0;
  for j = i + 1: n
    aux = aux + A(i,j) * x(j);
  end
  x(i) = ( A(i, n + 1) - aux ) / A(i,i);
end
x

% Cálculo do resíduo
for i = 1: n
  aux = 0;
  for j = 1: n
    aux = aux + Aaux(i,j) * x(j);
  end
  res(i) = abs(aux - Aaux(i, n + 1));
end
resMax = max( res )




