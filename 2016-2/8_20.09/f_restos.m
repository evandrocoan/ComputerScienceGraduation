function R = f_restos(n, a , xi)
a
n
for ndiv = 1 : 2
%Primeira divisão
    ndiv
    b(1)=a(1);
    for i=2 : n+1
        b(i) = a(i) + xi*b(i-1);
    end
    R(ndiv)= b(n+1);
%Fim da primeira divisão
    a = b;
    n = n-1;
end
end