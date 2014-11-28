clear
n = 10000;
t(1)=0;
for i=2:n
	t(i)=1;
end
r(1)=1;r(n)=1;
d(1)=1;d(n)=0;
for i=2:n-1
	r(i)=9;
	d(i)=1;
end
b(1)=150;b(n)=300;
for i=2:n/2
	b(i)=100;
end
for i=n/2+1:n-1
	b(i)=200;
end

x = trid(n,t,r,d,b)




