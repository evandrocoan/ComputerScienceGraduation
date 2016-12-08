function A = fpartial_pivot(n,A,k)
%Procura por todas as linhas pelo melhor pivot para cada passo
    maior_valor = abs(A(k,k)); 
    max_index = k;
 
    for i = k+1 : n
        aux = abs(A(i,k));
        if (aux > maior_valor)
            maior_valor = aux;
            max_index = i;
        end
    end
     
    Aaux = A(k,:);
    A(k,:) = A(max_index,:);
    A(max_index,:) = Aaux;
     
end