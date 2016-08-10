clear
N = 10000;

erro = 1;
cont = 0;
xp=[];
yp=[];
relax=1;#diagonal f-dominante
for i = 1:N
	xa(i) = 0;
end
while erro > 1e-6 && cont < 1000
	i = 1;
		x(i) = (1-relax)*xa(i)+relax*(150 - xa(i+1));
	for i = 2:N/2
		x(i) = (1-relax)*xa(i)+relax*(100 - (x(i-1) + xa(i+1)))/9;
	end
	for i = (N/2)+1:N-1
		x(i) = (1-relax)*xa(i)+relax*(200 - (x(i-1) + xa(i+1)))/9;
	end
	i = N;
		x(i) = (1-relax)*xa(i)+relax*(300 - (x(i-1)));

	erro = max(abs(x.-xa));
	xa = x;
	cont++;
	xp=[xp cont];
	yp=[yp x(1)];
	[cont x(N-1) x(N) erro]
end
plot(xp,yp);