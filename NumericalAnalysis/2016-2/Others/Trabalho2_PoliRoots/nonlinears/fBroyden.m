%Algoritmo de Newton e Broyden, com Jacobiana exata e com derivadas numéricas
% para n=2 equações
function [ x1, k, dif, residuo_sistema ] = fBroyden( x0 )
    
    X0  = transpose(x0);
    dif = 1;
    tol = 1e-14;
    
    k  = 0;
    dx = 0.1*( [ tol, tol ] ); %valor inicial de dx
    
    %%J0
    x1dx = [ x0(1) + dx( 1 ),  x0( 2 ) ]; %vetor x(1) com incremento
    x2dx = [ x0(1),  x0( 2 ) + dx( 2 ) ]; %vetor x(2) com incremento
    
    f1xi = f1( x0 ); %f1 aplicada no ponto inicial xi
    f2xi = f2( x0 ); %f2 aplicada no ponto inicial xi
    F0   = [ f1xi; f2xi; ];
    
    J( 1, 1 ) = ( f1(x1dx) - f1xi ) / dx(1);
    J( 1, 2 ) = ( f1(x2dx) - f1xi ) / dx(2);
    J( 2, 1 ) = ( f2(x1dx) - f2xi ) / dx(1);
    J( 2, 2 ) = ( f2(x2dx) - f2xi ) / dx(2);
    
    Jinv = inv( J );
    %J*Jinv %afericao
    
    k   = 0;
    dif = 1;
    
    DX = -Jinv*F0;
    X1 =  X0 + DX;
    x1 =  transpose( X1 );
    
    F1 = [ f1(x1); f2(x1); ];
    DF = F1 - F0;
    
    while dif > tol && k < 30
        
        k    = k + 1;
        Jinv = Jinv+ ( ( DX - ( Jinv*DF ) )*transpose( DX ) )*Jinv / ( transpose( DX )*Jinv*DF );
        
        DX = -Jinv*F1;
        
        X2 = X1 + DX;
        x1 = transpose( X2 );
        F2 = [ f1(x1); f2(x1); ];
        
        DF = F2 - F1;
        X1 = X2;
        F1 = F2;
        
        #dif = min( abs( DX ) ); %Critério ‘min’ pois dx é denominador
        dif = abs( DX(1) + DX(2) );
        
    end
    
    residuo_sistema = max( abs( f1( x1 ) ), abs( f2( x1 ) ) );
    
end

function x = f1( x0 )

    x1 = x0( 1 );
    x2 = x0( 2 );

    x = sin( x1 ) + cos( x2 ) - 1;

end

function x = f2( x0 )

    x1 = x0( 1 );
    x2 = x0( 2 );

    x = x1^2 + x2^2 - 3;

end
