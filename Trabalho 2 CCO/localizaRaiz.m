function [ xInicio xFinal ] = localizaRaiz( A, B ) 
  h = .1;
  ir = 1;
  
  % vari�vel para o c�lculo do comportamento da 
  % descontinuidade assint�tica: x != pi / 2 + pi k
  k = 0;
  a = A;
  b = a + h;

  passos = 0;

  % indice do perigo utilizado
  p = 1;

  %enquano bzinho n�o atingir o o bz�o
  while( b < B - h & ir < 11 & passos < 10000 )
    passos = passos + 1;
      
    %verifica se uma ra�z foi encontrada
    produto = f(a) * f(b);

    if( ( produto < 0 ) & ( fRegiaoCritica( f(a), f(a), h ) ) )
      % incrementa a ra�z 
      %ir++;
      xInicio(ir) = a;
      xFinal(ir) = b;
      ir = ir + 1;
    end

    %faz seguir para o teste do pr�ximo intervalo
    a = b;
    b = a + h;
  end
end
