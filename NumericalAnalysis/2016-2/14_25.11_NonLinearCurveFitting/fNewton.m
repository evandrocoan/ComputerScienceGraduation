function x = fNewton(xi)
    %primeira iteração 
    dif = 1.; 
    k = 0.; 
    dx = [1e-6 1e-6];
    while (dif > 1.e-14 && k <20) 
        k=k+1;
        A(1,1) = (f1([xi(1)+dx(1) xi(2)      ])-f1(xi))/dx(1); 
        A(1,2) = (f1([xi(1)       xi(2)+dx(2)])-f1(xi))/dx(2); 
        A(1,3) = -f1(xi); 
        A(2,1) = (f2([xi(1)+dx(1) xi(2)      ])-f2(xi))/dx(1); 
        A(2,2) = (f2([xi(1)       xi(2)+dx(2)])-f2(xi))/dx(2); 
        A(2,3) = -f2(xi); 
        dx=fGauss(2, A); % função que calcula a solução dos sistema pelo metodo de Gauss 
        x = xi + dx;
        
        %segunda iteracao 
        xi=x; 
        dif = min(abs(dx)); %Usa-se a mínima diferença para calcular as derivadas 
    end%while 
    dif
    k
end
