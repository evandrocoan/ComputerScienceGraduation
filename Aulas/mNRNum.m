clear
format long
a=1.5; b=2.0;
erro = 1.0;
cont = 0.0;
xi = 1.75;
dxi = 1.e-3;#2iteracoes converge

while( erro > 1.e-6 & cont < 100 )
    cont = cont + 1

    fln = (f(xi+dxi)-f(xi))/dxi
	dx=-f(xi)/fln	
	x=xi+dx;
	xi=x;
	dxi=dx;
    
	erro=abs(f(x))
end
printf("\n A raiz de f(x)=x.ln(x)-1 eh %.10f com criterio de parada |f(x)| <= %.10f com %d iteracoes\n\n",x,erro,cont);