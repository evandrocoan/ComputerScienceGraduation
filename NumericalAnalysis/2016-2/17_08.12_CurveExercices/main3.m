m=9
x=[0    0.2  0.4  0.6  0.8   1.0   1.2   1.4   1.6]
y=[0.9  1.1  1.2  1.3  1.4   1.4   1.3   1.2   1.1]

xi = x(1);
p2 = fliplr(polyfit(x, y, 2))
p3 = fliplr(polyfit(x, y, 3))

xp = min(x):0.02:max(x);
yp2 = fPnH(2, p2, xp);
yp3 = fPnH(3, p3, xp);

%plot(x, y, '*', xp, yp2, '-r', xp, yp3, '-.b')

a = fQ3b(m, x, y)
yp4 = a(1).*sin(xp) + a(2).*cos(xp);

a = fQ3c(m, x, y)
yp5 = sin(a(1).*xp) + cos(a(2).*xp);

'desvios locais em 3c:'
d5=sin(a(1).*x) + cos(a(2).*x) .-y
mediad5=sum(d5)/m %deve ser proxima de zero

plot(x, y, '*', xp, yp2, '-r', xp, yp3, '-.b', xp, yp4, '-..g', xp, yp5, '-m')