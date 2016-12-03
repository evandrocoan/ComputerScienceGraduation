function A = pivot_parcial(A, n, k) 

% Procura sempre a melhor linha para cada passo
% Descobrir melhor linha k, maior pivô (modulo) da coluna
    maior_valor = abs(A(k,k)); % k é sempre o maior valor
    max_index = k;
    
    for i = k+1 : n
        aux = abs(A(i,k)); % Pesquisa na linha certa
        if(aux > maior_valor)
            maior_valor = aux;
            max_index = i;
        end
    end
    
% Retorno
    Aaux = A(k,:); # ":" representa toda a linha(todo o j de uma matriz A(i, j))
    A(k,:) = A(max_index,:);
    A(max_index,:) = Aaux;
end