function residuo_max = rmax(A,n,x)
for i = 1:n
    soma = 0;
    for j = 1:n
        soma += A(i,j)*x(j);
    endfor
    r(i) = abs(soma - A(i,n+1));
endfor
residuo_max = max(r);
#residuo da double

end
