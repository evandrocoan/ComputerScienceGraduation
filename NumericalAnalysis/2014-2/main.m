%clc
clear
format long

a = [ 7.3  5.4  complex( 0, -1 )   -2 -3.2  7 ]; % coeficientes do polinômio
b = [ 1 1 1 1 1 1 ];
c = [ 1 -3  3  -1 ];%(x-1)³=0 -> raizes={1 1 1} x=1 com M=3 (multiplicidade)
d = [ 1 -5.4 12.15 -14.58 9.8415 -3.54294 0.531441];
%x^6-5.4x^5+12.15x^4-14.58x^3+9.8415x^2-3.54294x+0.531441 = 0
%coef=[1 -5.4 +12.15 -14.58 +9.8415 -3.54294 +0.531441];%(x-0.9)^6->raiz=0.9->M=6
%x^7-4.4x^6+6.75x^5-2.43x^4-4.7385x^3+6.29856x^2-3.011499x+0.531441=0
%coef=[1 -4.4 +6.75   -2.43 -4.7385 +6.29856 -3.011499 +0.531441];%(x+1)^1*(x-0.9)^6
%(x^8-3.4x^7+2.35x^6+4.32x^5-7.1685x^4+1.56006x^3+3.287061x^2-2.480058x+0.531441)=0
%coef=[1 -3.4 +2.35   +4.32 -7.1685 +1.56006 +3.287061 -2.480058 +0.531441 ];%(x+1)^2*(x-0.9)^6
%(x^8-3.4x^7+2.35x^6+4.32x^5-7.1685x^4+1.56006x^3+3.287061x^2-2.480058x+0.531441)*x²=0
coef=[1 -3.4 +2.35   +4.32 -7.1685 +1.56006 +3.287061 -2.480058 +0.531441 0 0];%(x+1)^2*(x-0.9)^6*(x-0)²
raiz = roots2( coef )
roots(coef)
