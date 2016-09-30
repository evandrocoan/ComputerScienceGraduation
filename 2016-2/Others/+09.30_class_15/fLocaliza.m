function xi = fLocaliza( n, a )
    
    # Encontrar o raio inicial
    raio_inicial = 1 + max( abs( a(2:n+1) ) ) / abs( a(1) )
    
    # Encontrar o raio mínimo das raízes. Abaixo dele não existe raízes.
    raio_minimo = 1 / ( 1 + max( abs( a(1:n) ) ) / abs( a(n+1) ) )
    
    xi = ( raio_inicial + raio_minimo ) / 2
    
    
end
