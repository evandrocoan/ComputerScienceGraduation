function x = fgauss(N,A)
A = escalonamento(N,A);
x = retrosubs(N,A);
end #function

function A = escalonamento(N,A)
for k=1:N-1
    #A = pivotacao(N,A,k);
    for i=k+1:N
        aux = A(i,k)/A(k,k);
        for j=k+1:N+1
            A(i,j)= A(i,j)- aux * A(k,j);
        end #for j
        A(i,k)= 0;
    end #for i
end #for k
end #function

function x = retrosubs(N,A)
x(N) = A(N, N+1)/A(N,N);
for i=N-1:-1:1
    soma =0;
    for j=i+1:N
        soma = soma + A(i,j)*x(j);
    end #for j
    x(i) = (A(i, N+1)- soma)/A(i,i);
end #for i
end #function

function A = pivotacao(N,A,k)
    amax = abs(A(k,k));
    imax = k;
    for i=k+1:N
        if abs(A(i,k)) > amax
            amax = abs(A(i,k));
            imax = i;
        end %if
    end %for
    aux = A(k,:);
     A(k,:) = A(imax, :);
    A(imax,:) = aux;
end %function


