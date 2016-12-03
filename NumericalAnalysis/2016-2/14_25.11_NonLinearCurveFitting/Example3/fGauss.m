function x = fGauss(n, A) % Com pivotação parcial

% Escalonamento para transformar A em U (triangular superior)
% Troca de linhas e escolhe a melhor linha 1

    for k = 1 : n - 1
        for i =  k + 1 : n
            % Antes de usar, escolhe o melhor pivô
            % k indica qual linha quer melhorar
            
            A = pivot_parcial(A,n,k); %imprime com linhas trocadas
            
            aux = A(i, k)/A(k, k);
            for j = k + 1 : n + 1
                A(i,j) = A(i, j) - aux * A(k, j); %Li <- Li - aux * Lk
            end 
            
            A(i,k) = 0; % Coluna j=k tem resultado ZERO conhecido
            
        end
    end
   
% Retrosubstituição
% Último x é o primeiro a ser calculado
% Testar o sistema
    aux_abs = abs(A(n,n));
    if (aux_abs < 1e-15)
        if (abs(A(n,n+1)) < 1e-15)
        % Atribuir valor a x(n)
            x(n) = 0; printf('Sistema Possível Indeterminado\n')
        else
            x(n) = NaN; printf('Sistema Impossível\n')
        end
    else
        x(n) = (A(n,n+1)/A(n,n));
    end
    
    for i=(n-1):-1:1
        soma = 0;
        for j=(i+1):n
            soma = soma + (A(i,j) * x(j));
        end %j
        x(i) = (A(i, n+1) - soma)/ A(i,i);
    end %i
end