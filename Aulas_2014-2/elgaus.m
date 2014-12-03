%eliminacao gausciana
n=3;
A=[-1.1,2,3,4;
   5,6.2,7,8;
   9,0.5,10,2;];

original = A;

for k = 1 : n - 1
    for i = k + 1 : n
        aux = A(i,k)/A(k,k);
        A(i,k) = 0;
        for j = k + 1 : n + 1
            A(i,j) = A(i,j) - aux*A(k,j);
        end
    end
end

X(n) = A(n,n+1) / A(n,n);
for i = n-1 : -1 : 1
    aux = 0;
    for j = i + 1 : n
        aux = aux + A(i,j) * X(j);
    end
    X(i) = (A(i,n + 1) - aux)/ A(i,i);
end

for i = 1 : n
    aux = 0;
    for j = 1 : n
        aux = aux + original(i,j) * X(j);
    end
    res(i) = abs(aux - original(i,n + 1));
end

resMax = max( res );
