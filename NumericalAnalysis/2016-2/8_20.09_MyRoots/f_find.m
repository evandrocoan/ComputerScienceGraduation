function xi = f_find (n , a)

% Encontrar o raio inicial das raizes. 
    raio_init = 1 + max(abs(a(2:n+1))) / abs(a(1))
    
% Encontrar o raio minimo das raizes.  Abaixo dele nao existe raizes
    raio_min = 1/ (1 +max(abs(a(2:n+1)))) / abs(a(n+1))
  
    raio_med = (raio_init + raio_min)*0.5
    xi=complex( raio_med*cos(pi/4),raio_med*sin(pi/4))
    xi=2%teste
 
end