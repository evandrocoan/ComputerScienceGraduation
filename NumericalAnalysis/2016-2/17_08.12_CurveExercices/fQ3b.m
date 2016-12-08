function a = fQ3b(m, x, y)
    A(1,1) = sum(sin(x).^2);
    A(1,2) = sum(sin(x).*cos(x));
    A(1,3) = sum(y.*sin(x));
    A(2,1) = A(1,2);
    A(2,2) = sum(cos(x).^2);
    A(2,3) = sum(y.*cos(x));
    
    a = fGauss(2, A);
end %function