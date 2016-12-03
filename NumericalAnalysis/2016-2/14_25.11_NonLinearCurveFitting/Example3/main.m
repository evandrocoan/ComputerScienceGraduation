m = 6;
x = [ 0.2    0.4    0.6    0.8   0.9   1.0  ];  %nem sempre estão ordenados
y = [ 0.04   0.14   0.30   0.45  0.61  0.69 ];  % medidos

ai = [1 1];    %chute inicial influencia muito na obtenção do resultado final
a = fNewton(ai)
f1(a)
f2(a)
xp = min(x) : 0.05 : max(x);
yp = log(a(1) .+ a(2) .* xp.^2);

plot(x, y, '*', xp, yp, 'm', 2, 1)