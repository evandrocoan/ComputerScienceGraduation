clear
a = 0.1; b = 3.0; h= 0.1;
x=a:h:b;
y = x.*log(x).-1;#. = trabalha com conjunto de valores
plot(x,y)
grid
