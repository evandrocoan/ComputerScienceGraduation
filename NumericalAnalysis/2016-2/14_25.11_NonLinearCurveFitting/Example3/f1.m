function z = f1(a)
    m = 6;
    x = [ 0.2    0.4    0.6    0.8   0.9   1.0  ];  %nem sempre estão ordenados
    y = [ 0.04   0.14   0.30   0.45  0.61  0.69 ];  % medidos
    z = 0;
    
    for k = 1: m    %somatório
        z += ( log(a(1) + a(2)*x(k)^2) - y(k)) / (a(1) + a(2)*x(k)^2);
    end
end