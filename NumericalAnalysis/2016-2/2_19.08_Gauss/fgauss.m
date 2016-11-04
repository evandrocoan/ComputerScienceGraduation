function x = fgauss(A, n)

%Escalonamento para transformar A em U (triangular superior)
%troca de linhas e escolhe a melhor linha 1
    for k = 1 : n - 1
        
        processing_step = k
        #A = pivot_parcial(A,n,k); #imprime com linhas trocadas
        #Matrix = A
        
        for i =  k + 1 : n
            %resolver o probs do zero
            #antes de usar, escolhe o melhor pivô
            #k indica qual linha quer melhorar
            current_line = i
            
            dividendo = A(i, k)
            divisor   = A(k, k)
            aux       = dividendo / divisor
            
            A(i,k) = 0; %coluna j=k tem resultado ZERO conhecido
            Matrix = A
            
            for j = k + 1 : n + 1
                
                non_zero = j
                A(i,j)   = A(i, j) - aux*A(k, j); %Li <- Li - aux * Lk
                Matrix   = A
                
            end 
            
        end
       
    end
    
    Matrix = A
    
%retrosubstituição
#ultimo x é o primeiro a ser calculado
#testar o sistema
aux_abs = abs(A(n,n));
if (aux_abs < 1e-15)
    if (abs(A(n,n+1)) < 1e-15)
    #atribuir valor a x(n)
        x(n) = 0; "Sistema Possível Indeterminado"
    else
        x(n) = NaN; "Sistema Impossível"
    endif
else
    x(n) = (A(n,n+1)/A(n,n));

endif


printf( '\n\n\n\n\n\nStarting the retro_substitution...\n\n' )

for i=(n-1):-1:1
    current_line = i
    sum = 0
    for j=(i+1):n
       non_zero = j
       multiplado = A(i,j)
       multiplicador = x(j)
       sum = sum + (A(i,j) * x(j))
    end %j
    x(i) = (A(i, n+1) - sum)/ A(i,i)
end %i

%   x

end
