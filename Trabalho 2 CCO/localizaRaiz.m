function [ xInicio xFinal ] = localizaRaiz( A, B ) 
  h = .1;
  ir = 0;
  
  % variável para o cálculo do comportamento da 
  % descontinuidade assintótica: x != pi / 2 + pi k
  k = ir;
  a = A;
  b = a + h;
  %enquano bzinho não atingir o o bzão
  while( b < B - h )
    produto = f(a) * f(b);
    perigo = ( pi / 2 ) + pi * k;
    % ( perigo - h < a < perigo + h ) isola da zona de divergência
    % produto < 0 informa que há uma raíz no intervalo
    printf("( perigo - h > a )") ( perigo - h > a )
    printf("( a > perigo + h )") ( a > perigo + h )
    if( produto < 0 & ( perigo - h > a ) & ( a > perigo + h ) )
      ir = ir + 1;
      xInicio(ir) = a;
      xFinal(ir) = b;
    end
    a = b;
    b = a + h;
  end
end
