clear
format long

n=6 #n tbm pode ser length(a)-1
nRaizes = n;
a=[1 -5.4 12.15 -14.58 9.8415 -3.54294 0.531441]
%a=[1 -3 3 -1]
coef =a;
xi = [complex(1,1), complex(1,1), complex(1,1), complex(1,1), complex(1,1), complex(1,1)]
j = 1;
erro = 0;
while (j <= nRaizes && erro <= 1.e-12)
	a;
	[x(j) erro cont] = fNRPM(n,a,xi(j));
	if erro <= 1.e-12
		printf("\n A %da. raiz de Pn(x) eh %.10f+%.10fi com criterio de parada |Pn(x)| <= %.10f com %d iteracoes\n\n",j,real(x(j)),imag(x(j)),erro,cont);
	end
	[x(j) erro cont] = fNRPM(nRaizes,coef,x(j));#refinamento das raizes encontradas
	if erro <= 1.e-12
		printf("\n A %da. raiz refinada de Pn(x) eh %.10f+%.10fi com criterio de parada |Pn(x)| <= %.10f com %d iteracoes\n\n",j,real(x(j)),imag(x(j)),erro,cont);
	end
	b(1) = a(1);
	for i = 2 : n+1
		b(i) =a(i) +x(j)*b(i-1);	
	end
	R1 = b(n+1);	
	n = n -1;
	a = b;
	j = j+1;
#fim do bloco
end

