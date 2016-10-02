function [ xInicio xFinal ] = localizaRaiz( A, B ) 
  h = .1;
  ir = 1;
  
  % variável para o cálculo do comportamento da 
  % descontinuidade assintótica: x != pi / 2 + pi k
  k = 0;
  a = A;
  b = a + h;

  passos = 0;

  % indice do perigo utilizado
  p = 1;

  %enquano bzinho não atingir o o bzão
  while( b < B - h && ir < 11 && passos < 10000 )
    passos = passos + 1;
      
    %verifica se uma raíz foi encontrada
    produto = f(a) * f(b);

    if( ( produto < 0 ) && ( fRegiaoCritica( f(a), f(a), h ) ) )
      % incrementa a raíz 
      %ir++;
      xInicio(ir) = a;
      xFinal(ir) = b;
      ir = ir + 1;
    end

    %faz seguir para o teste do próximo intervalo
    a = b;
    b = a + h;
  end
end
