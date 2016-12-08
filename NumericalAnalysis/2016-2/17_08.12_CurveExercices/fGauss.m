function x = fGauss(n,A)

    %Triangulização
    for k = 1 : n - 1

        %Pivotamento parcial
        A = fpartial_pivot(n,A,k);
     
        for i = k+1 : n
            aux = A(i,k) / A(k,k);
            for j = k+1 : n+1
                A(i,j) = A(i,j) - aux*A(k,j);   %Li <- Li - aux*Lk
            end
        A(i,k) = 0;                             %coluna j = k tem resultado 0 conhecido
        end
    end
     
    
    %Retrosubstituição
    
       if (abs(A(n,n)) < 1e-14)
        if (abs(A(n,n+1)) < 1e-14)
            %Atribuir valor a x(n)
            x(n) = 0; "Sistema Possível Indeterminado"
        else
            x(n) = NaN; "Sistema Impossível"
        end
    else
        x(n) = A(n,n+1)/A(n,n);
    end
 
    for i = n-1 : -1 : 1
        soma = 0;
        for j = i+1 : n
            soma = soma + (A(i,j) * x(j));
        end
    x(i) = (A(i,n+1)- soma) / A(i,i);
    end
     
 
end