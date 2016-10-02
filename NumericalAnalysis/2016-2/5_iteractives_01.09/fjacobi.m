function x=fjacobi(n,A,xi,tol)

k=0;
diferenca=1;

while(diferenca>tol)

k=k+1;

x(1)=(A(1,4)-A(1,2)*xi(2)-A(1,3)*xi(3))/A(1,1);
x(2)=(A(2,4)-A(2,1)*xi(1)-A(2,3)*xi(3))/A(2,2);
x(3)=(A(3,4)-A(3,1)*xi(1)-A(3,2)*xi(2))/A(3,3);

diferenca= max(abs(x .- xi));

xi=x;
end %while
k;
diferenca;
end