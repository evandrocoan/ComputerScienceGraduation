function A = fPivotParcial(n, A, k)
    maior = abs(A(k,k));
    imaior = k;
    for i = k+1 : n
        if abs(A(i,k)) > maior
            maior = abs(A(i,k));
            imaior = i;
        end
    end
    lAux = A(k,:); %salvando linha antiga
    A(k,:) = A(imaior,:); %trocando as linhas
    A(imaior,:) = lAux; %retornando a linha antiga para a nova posição
end