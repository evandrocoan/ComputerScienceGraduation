clear
N = 10000;

erro = 1;
cont = 0;

for i = 1:N
	xa(i) = 0;
end
while erro > 1e-6 && cont < 100
	i = 1;
		x(i) = 150 - xa(i+1);
	for i = 2:N/2
		x(i) = (100 - (xa(i-1) + xa(i+1) + xa(i+100)))/9;
	end
	for i = (N/2)+1:N-1
		x(i) = (200 - (xa(i-100) + xa(i-1) + xa(i+1)))/9;
	end
	i = N;
		x(i) = 300 - (xa(i-1));

	erro = max(abs(x.-xa));
	xa = x;
	cont++;
	[cont x(1) x(N) erro]
end