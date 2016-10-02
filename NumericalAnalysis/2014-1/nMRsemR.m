clear
format long

n=5 #n tbm pode ser length(a)-1
nRaizes = n;
a=[1 1 1 0 2 1]
coef =a;
xi = [-1 complex(1,1) complex(1,-1) complex(1,2) complex(1,-2)]
j = 1;
erro = 0;
while (j <= nRaizes & erro <= 1.e-6)
	a
	[x(j) erro cont] = fNRP(n,a,xi(j));
	#[x(j) erro cont] = fNRP(nRaizes,coef,x(j));#refinamento das raizes encontradas
	#b(1) = a(1);
	#for i = 2 : n+1
	#	b(i) =a(i) +x(j)*b(i-1);	
	#end
	#R1 = b(n+1);	
	if erro <= 1.e-6
		printf("\n A %da. raiz de Pn(x) eh %.10f+%.10fi com criterio de parada |Pn(x)| <= %.10f com %d iteracoes\n\n",j,real(x(j)),imag(x(j)),erro,cont);
	end
	#n = n -1;
	#a = b;
	j = j+1;
#fim do bloco
end
