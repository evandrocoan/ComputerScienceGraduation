clear
clc
%y''' + y'' + y'+y=0 -> y'''=-y''-y'-y
%y(x=0)=1
%y'(x=0)=-1
%y''(x=0)=1
%--------------
%y1=y   -> y1'=y2		=h1(x,y1,y2,y3)  -> y1(x=0)=1
%y2=y'  -> y2'=y3		=h2(x,y1,y2,y3)	 -> y2(x=0)=-1	
%y3=y'' -> y3'=-y3-y2-y1=h3(x,y1,y2,y3)  -> y3(x=0)=1
%--------------
a=0;
b=1;
x(1)=0;y1(1)=1;y2(1)=-1;y3(1)=1;
%1º Calculo
n=8;
[x y1 y2 y3]=RK4sist(a,b,n,x(1),y1(1),y2(1),y3(1));
%2º Calculo: calcular o valor exato "estimado" de y(valores de referencia)
n2=2*n;
[x2 y12 y22 y32]=RK4sist(a,b,n2,x(1),y1(1),y2(1),y3(1));
erroestimadomax=abs(y1(n+1)-y12(n2+1))
ye=exp(-x);
%solucao da equacao diferencial de terceira ordem e y(x):
y=y1;
erroexatomax=abs(y1(n+1)-ye(n+1))
plot(x,ye,"b;y(x)=exp(-x)->solucao exata;",x,y,"r;solucao aproximada por RK4;")