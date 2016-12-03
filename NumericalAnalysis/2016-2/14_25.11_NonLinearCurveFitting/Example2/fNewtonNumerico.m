function X = fNewtonNumerico(n,Xi) 
    tol = 1e-14; 
    criterio = 1;
    contador = 0; 
    for i = 1:n 
        Dx(i)=1e-6; 
    end 
    while (criterio > tol && contador < 50) 
        contador = contador + 1; 
        % Gerando a jacobiana 
        Yi = F(Xi); % Valor inicial 
        Xj = Xi; 
        for j = 1:n % varre as colunas 
            Xj(j) = Xi(j) + Dx(j); % incremento na coluna j 
    %            for i=1:n 
                    A(:,j)=(F(Xj) .- Yi)/Dx(j);  
    %            end 
                Xj = Xi; % volta ao valor original 
        end 
        A(:,n+1) = -Yi; 
        Dx = fGaussPivot(n,A); 
        X = Xi + Dx; 
        Xi = X; 
        criterio = min(abs(Dx)); 
    end 
    contador 
    criterio 
end 