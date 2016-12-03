function z = F(a) 
    m = 10;
    x = [1 2.6 3.5 4.1 5.5 6.5 7.1 8.0 3.4 1.2];
    y = [0.1 0.2 0.25 0.3 0.4 0.5 0.41 0.35 0.24 0.15];
    
    % f1 - primeira equação
    z(1) = 0;
    for k = 1 : m
        z(1) = z(1) + (a(1) + a(2)*sin(a(3) + a(4)*x(k)) - y(k));
    end
    % f2 - segunda equação
    z(2) = 0;
    for k = 1 : m
        z(2) = z(2) + (a(1) + a(2)*sin(a(3) + a(4)*x(k)) - y(k)) * sin(a(3) + a(4)*x(k));
    end
    % f3 - terceira equação
    z(3) = 0;
    for k = 1 : m
        z(3) = z(3) + (a(1) + a(2)*sin(a(3) + a(4)*x(k)) - y(k)) * a(2) * cos(a(3) + a(4)*x(k));
    end
    % f4 - quarta equação
    z(4) = 0;
    for k = 1 : m
        z(4) = z(4) + (a(1) + a(2)*sin(a(3) + a(4)*x(k)) - y(k)) * a(2) * cos(a(3) + a(4)*x(k)) * x(k);
    end 
end 