% g(x) = a(1)*x + a(2)*cos(x)

function a = fAjusteCos(m, x, y)
    vcos = [cos(x)];
    A(1,1) = sum(x.*x);
    A(1,2) = sum(x.*vcos);
    A(2,1) = A(1,2);
    A(2,2) = sum(vcos.*vcos);
    A(1,3) = sum(x.*y);
    A(2,3) = sum(y.*vcos);
    A;
    a = fGaussPivot(2, A);
end