
function coeficientes = ajusteDeCurvasLinearesParaCos( m, x, y )

    vcos = [cos(x)];

    A(1,1) = sum( sin(x).^2 );
    A(1,2) = sum( log(x).*sin(x) );
    A(1,3) = sum( y.*sin(x) );

    A(2,1) = sum( sin(x).*log(x) );
    A(2,2) = sum( log(x).^2 );
    A(2,3) = sum( y.*log(x) );

    A;
    coeficientes = fgauss(2, A);

end



