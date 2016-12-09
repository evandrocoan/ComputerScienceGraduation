

# Derivada de \sum em relação ao a(1)
function summation = f1Problema1( a )

    x=[0.1 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6];
    y=[0.1 0.9 1.7 2.3 2.8 3.1 3.4 3.6 3.7];

    m = numel( y );
    summation = 0;

    for k = 1 : m

        summation = summation + ( sin(a(1)*x(k)) + log(a(2)*x(k)) - y(k) )*(cos(x(k)*a(1))*x(k));

    end

end


