function [x] = fjacobi (n,A,xi,tol)
dmax=1;
k=0;
while dmax>tol
k=k+1;
x(1)=(A(1,n+1)-A(1,2)*xi(2)-A(1,3)*xi(3))/A(1,1);
x(2)=(A(2,n+1)-A(2,1)*xi(1)-A(2,3)*xi(3))/A(2,2);
x(3)=(A(3,n+1)-A(3,1)*xi(1)-A(3,2)*xi(2))/A(3,3);
dmax=max(abs(x-xi));
xi=x;
end
"numero de repeticoes= " 
k
"criterio de parada= " 
dmax 
end%function