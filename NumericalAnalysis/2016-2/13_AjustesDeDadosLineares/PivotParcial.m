function A = PivotParcial(n,A,k)
    % Passo 1: escolher e localizar o maior modulo da coluna j=k
    maior_valor = abs(A(k, k));     % Supoe que o primeiro é o maior valor
    indice_max = k;                 % Guarda indice do maior valor
    for i = k+1 : n                 % Procura o melhor pivo para cada linha i
        aux = abs(A(i, k));
        if(aux > maior_valor)
            maior_valor = aux;
            indice_max = i;
        end
    end
    
    % Passo 2: Troca de linhas, entre a linha i=k e a linha i=indice_max
    Aaux = A(k,:);
    A(k,:) = A(indice_max,:);
    A(indice_max,:) = Aaux;
end