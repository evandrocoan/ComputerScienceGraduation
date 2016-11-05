function [x cont k diferenca]=fgaussseidel(n,A,xi,tol)

k=0;
diferenca=1;
x=xi;
fator=0.86;
aux=1-fator; %primeira operacao
cont=1;
while(diferenca>tol)

k=k+1;

x(1)=(aux)*xi(1) + (fator*(A(1,4)-A(1,2)*x(2)-A(1,3)*x(3))/A(1,1));
cont=cont+8;
x(2)=(aux)*xi(2) + (fator*(A(2,4)-A(2,1)*x(1)-A(2,3)*x(3))/A(2,2));
cont=cont+8;
x(3)=(aux)*xi(3) + (fator*(A(3,4)-A(3,1)*x(1)-A(3,2)*x(2))/A(3,3));
cont=cont+8;
diferenca= max(abs(x .- xi));
cont=cont+n; %para soma: cont=cont+n+(n+1)
xi=x;
end %while
k;
diferenca;
cont;
end