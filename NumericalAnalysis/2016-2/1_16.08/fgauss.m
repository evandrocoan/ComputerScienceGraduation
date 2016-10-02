function x = fgauss(n,A)
%Escalonamento para transformar A em U (triangular superior)
    for k = 1 : n - 1
    
        for i =  k + 1 : n 
        
            for j = k + 1 : n + 1
        
                aux = A(i, k)/A(k, k);
                A(i,j) = A(i, j) - aux*A(k, j); %Li <- Li - aux * Lk
            
            end 
            
            A(i,k) = 0;%coluna j=k tem resultado ZERO conhecido
            
        end
       
    end
    
   % A

%retrosubstituição

    x(n) = (A(n,n+1)/A(n,n));

for i=(n-1):-1:1
    soma = 0;
    for j=(i+1):n
       soma = soma + (A(i,j) * x(j));
    end %j
  x(i) = (A(i, n+1) - soma)/ A(i,i);
end %i

%   x

end