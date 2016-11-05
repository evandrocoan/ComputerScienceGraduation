function x = fgauss(n,A)
%Escalonamento para transformar A em U (triangular superior)
cont=0;
    for k = 1 : n - 1
        k;
        A;
        A = fPivotParcial(n, A, k);
        
        for i =  k + 1 : n 
        
            aux = A(i, k)/A(k, k);
            cont=cont+1;
            for j = k + 1 : n + 1
        
                A(i,j) = A(i, j) - aux*A(k, j); %Li <- Li - aux * Lk
            cont=cont+2;
            end 
            
            A(i,k) = 0; %coluna j=k tem resultado ZERO conhecido
            
        end
        A;
    end
    

%retrosubstituiÃ§Ã£o para sistemas com uma linha como combinaÃ§Ã£o linear de outra(s)
    if abs(A(n,n))<1e-15
        if abs(A(n,n+1))<1e-15
            x(n)=1500; %escolher um valor de referencia
            "Sistema possivel e indeterminado"
        else
            x(n)=NaN;
            "Sistema impossi­vel"
        end
    else
    %   Sistema possi­vel e determinado
        x(n) = (A(n,n+1)/A(n,n));
        cont=cont+1;
    end
    
for i=(n-1):-1:1
    soma = 0;
    for j=(i+1):n
       soma = soma + (A(i,j) * x(j));
       cont=cont+2;
    end %j
  x(i) = (A(i, n+1) - soma)/ A(i,i);
  cont=cont+2;
end %i

%   x
printf("Número de operações teórico:")
oper=((4*n^3)+(9*n^2)-n-6)/6
cont
end