function Sn=fSn(n,a,b)
h=(b-a)/n;
x=a:h:b;
y=f(x);
somapar=0;
somaimpar=0;
for i=2:2:n
    somapar=somapar+y(i);
end
for i=3:2:n-1
somaimpar=somaimpar+y(i);
end
Sn=h/3*(4*somapar+2*somaimpar+y(1)+y(n+1));
end