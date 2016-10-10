function R = calcularRestoDaDivisao( n, a, xi )
    
    while( ndiv > 0 )
        
        # primeira divisão
        ndiv = 1
        b(1) = a(1)
        
        for i = 2 : n + 1
            
            b(i) = a(i) + xi*b(i-1)
            
        end
        
        R(ndiv) = b(n+1)
        # fim da primeira divisão
        
        # Brio-Rufini precisa do grau, e a cada passo, o grau do polinômio diminui o grau,
        # pois estamos dividindo.
        a    = b
        n    = n - 1
        ndiv = ndiv + 1
        
    end
    
end
