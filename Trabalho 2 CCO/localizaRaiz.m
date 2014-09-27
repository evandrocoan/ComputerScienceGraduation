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

  %incrementa o intervalo para cálculo do perigo
  % o ponto de perigo tem que ter uma resposta antes do inicio da calculo para
  % assim quando se chegar perto dele, poder pular.
  for ind = 1 : 11
    %calculo o perigo da zona de divergência
    perigos(ind) = ( pi / 2 ) + pi * ind
  end

  %enquano bzinho não atingir o o bzão
  while( b < B - h & ir < 10 & passos < 100 )
    passos = passos + 1;
      
    %verifica se uma raíz foi encontrada
    produto = f(a) * f(b);

    if( produto < 0 & ( perigos(p) - h > a ) )
      % incrementa a raíz 
      %ir++;
      xInicio(ir) = a
      xFinal(ir) = b
      ir = ir + 1

      % ajusta o perigo para o 'a' correto
      if( perigos(p) < a )
        %coloca o próximo perigo para o cálculo
        p = p + 1;
      end
    end

    %faz seguir para o teste do próximo intervalo
    a = b;
    b = a + h;
  end
end
