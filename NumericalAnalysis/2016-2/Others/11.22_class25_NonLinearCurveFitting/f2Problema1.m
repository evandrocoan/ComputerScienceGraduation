

# Derivada de \sum em relação ao a(2)
function summation = f2Problema1( a )

    y = [ 0.1, 0.3, 0.9, 1.2, 1.3, 1.2, 1 ];
    m = numel( y );

    x = [ 0 : 1 : (m-1) ];
    summation = 0;

    for k = 1 : m

        summation = summation + ( a(1) * sin(a(2) * x(k)) - y(k) ) * a(1) * (cos(a(2)*x(k))) * x(k);

    end

end




