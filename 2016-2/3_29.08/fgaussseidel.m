function [x] = fgaussseidel (n,A,xi,tol,lambda)
dmax=1;
k=0;
x=xi;
lambda1=(1-lambda);
while dmax>tol
k=k+1;
x(1)=lambda1*xi(1)+lambda*(A(1,n+1)-A(1,2)*x(2)-A(1,3)*x(3))/A(1,1);
x(2)=lambda1*xi(2)+lambda*(A(2,n+1)-A(2,1)*x(1)-A(2,3)*x(3))/A(2,2);
x(3)=lambda1*xi(3)+lambda*(A(3,n+1)-A(3,1)*x(1)-A(3,2)*x(2))/A(3,3);
dmax=max(abs(x-xi));
xi=x;
end
"numero de repeticoes "
 k
"criterio de parada " 
dmax 
end%function