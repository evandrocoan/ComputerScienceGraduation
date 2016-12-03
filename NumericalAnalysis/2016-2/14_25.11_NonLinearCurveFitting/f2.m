function z = f2(a)
    m = 7;
    x = [0:6];                        %nem sempre estão ordenados
    y = [0.1 0.3 0.9 1.2 1.3 1.2 1];  % medidos
    z = 0;
    for k = 1: m    %somatório
        z += ( a(1) * sin(a(2) * x(k)) - y(k) ) * a(1) * (cos(a(2)*x(k))) * x(k);
    end
end