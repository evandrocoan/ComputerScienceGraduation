function yp = fPnPorHorner( n, a, xp )
    
    
    for k = 1 : length( xp )
        
        # Precisamos limpar auxiliar a cada iteração
        aux = a( n + 1 );
        
        for i = n : -1 : 1
            
            aux = a( i ) + xp( k )*aux;
            
        end
        
        yp( k ) = aux;
        
    end
    
end



