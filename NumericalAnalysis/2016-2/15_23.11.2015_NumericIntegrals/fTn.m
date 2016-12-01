function Tn=fTn(n,a,b)
h=(b-a)/n;
x=a:h:b;
y=f(x);
Tn=0; %soma
for i=2:n
    Tn=Tn+y(i);
end
Tn=0.5*h*(2*Tn+y(1)+y(n+1));
end