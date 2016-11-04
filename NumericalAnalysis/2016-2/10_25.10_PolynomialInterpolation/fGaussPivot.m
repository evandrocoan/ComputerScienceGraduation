function x = fGaussPivot(n,A)
    cont = 0;    % Contador do numero de operacoes em Ponto Flutuante
    for k = 1 : n-1
        % Escolher o melhor pivo antes da eliminacao Gaussiana
        A = PivotParcial(n,A,k);    % matriz pivotada (com k a linha que se quer melhorar)
        for i = k+1 : n
            aux = A(i,k) / A(k,k);
            cont = cont + 1;
            for j = k+1 : n+1
                A(i,j) = A(i,j) - aux * A(k,j);    % Li <- Li - aux * Lk
                cont = cont + 2;     
            end
            A(i,k) = 0;    % Já sabemos que será zero em j=k
        end
    end
    A;    % Imprime A escalonada
   
    % Determina o tipo do sistema e faz retrosubstituição
    if (abs(A(n,n)) < 1e-14)        % Diagonal principal nula
        if (abs(A(n,n+1)) < 1e-14)  % Termo independente nulo
            x(n) = 0;  % "Sistema Possível Indeterminado"
            % Retrosubstituicao
            for i = n-1 : -1 : 1
                soma = 0;
                for j = i+1 : n
                    soma = soma + A(i,j) * x(j);
                    cont = cont + 2;
                end
                x(i) = (A(i,n+1) - soma) / A(i,i);
                cont = cont + 2;
            end
        else    % Termo independente não nulo 
            x(n) = NaN; % "Sistema Impossível"
        end
    else        % Diagonal principal não nula
        x(n) = A(n,n+1) / A(n,n);  %  "Sistema Possível Determinado"
        cont = cont + 1;    
        % Retrosubstituicao
        for i = n-1 : -1 : 1
            soma = 0;
            for j = i+1 : n
                soma = soma + A(i,j) * x(j);
                cont = cont + 2;
            end
            x(i) = (A(i,n+1) - soma) / A(i,i);
            cont = cont + 2;
        end
    end
    
    % printf("\n Numero de operações em Ponto Flutuante: %d\n", cont)
    cont_teorico = (4*n^3+9*n^2-n-6)/6;
    % printf("\n Contador correto de operações em Ponto Flutuante: %d\n", cont_teorico)
    x;
end