clear
format long
a=1.5; b=2.0;
erro = 1.0;
cont = 0.0;
xi = 1.75;

while( erro > 1.e-6 & cont < 100 )
    cont = cont + 1
    
	dx=-f(xi)/fl(xi)	
	x=xi+dx;
	xi=x;
    
	erro=abs(f(x))
end
printf("\n A raiz de f(x)=x.ln(x)-1 eh %.10f com criterio de parada |f(x)| <= %.10f com %d iteracoes\n\n",x,erro,cont);