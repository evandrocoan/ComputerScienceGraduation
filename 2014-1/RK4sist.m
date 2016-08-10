function [x y1 y2 y3]=RK4sist(a,b,n,xi,y1i,y2i,y3i)
x(1)=xi;y1(1)=y1i;y2(1)=y2i;y3(1)=y3i;
h=(b-a)/n;
for i=2:n+1
K1f1=h1(x(i-1),y1(i-1),y2(i-1),y3(i-1));
K1f2=h2(x(i-1),y1(i-1),y2(i-1),y3(i-1));
K1f3=h3(x(i-1),y1(i-1),y2(i-1),y3(i-1));

K2f1=h1(x(i-1)+0.5*h,y1(i-1)+0.5*h*K1f1,y2(i-1)+0.5*h*K1f2,y3(i-1)+0.5*h*K1f3);
K2f2=h2(x(i-1)+0.5*h,y1(i-1)+0.5*h*K1f1,y2(i-1)+0.5*h*K1f2,y3(i-1)+0.5*h*K1f3);
K2f3=h3(x(i-1)+0.5*h,y1(i-1)+0.5*h*K1f1,y2(i-1)+0.5*h*K1f2,y3(i-1)+0.5*h*K1f3);

K3f1=h1(x(i-1)+0.5*h,y1(i-1)+0.5*h*K2f1,y2(i-1)+0.5*h*K2f2,y3(i-1)+0.5*h*K2f3);
K3f2=h2(x(i-1)+0.5*h,y1(i-1)+0.5*h*K2f1,y2(i-1)+0.5*h*K2f2,y3(i-1)+0.5*h*K2f3);
K3f3=h3(x(i-1)+0.5*h,y1(i-1)+0.5*h*K2f1,y2(i-1)+0.5*h*K2f2,y3(i-1)+0.5*h*K2f3);

K4f1=h1(x(i-1)+h,y1(i-1)+h*K3f1,y2(i-1)+h*K3f2,y3(i-1)+h*K3f3);
K4f2=h2(x(i-1)+h,y1(i-1)+h*K3f1,y2(i-1)+h*K3f2,y3(i-1)+h*K3f3);
K4f3=h3(x(i-1)+h,y1(i-1)+h*K3f1,y2(i-1)+h*K3f2,y3(i-1)+h*K3f3);

	x(i)=x(i-1)+h;
	y1(i)=y1(i-1)+(K1f1+2*(K2f1+K3f1)+K4f1)/6*h;
	y2(i)=y2(i-1)+(K1f2+2*(K2f2+K3f2)+K4f2)/6*h;
	y3(i)=y3(i-1)+(K1f3+2*(K2f3+K3f3)+K4f3)/6*h;
end
end
