clear
format long
a=1.5; b=2.0;
erro = 1.0;
cont = 0.0;

while( erro > 1.e-6 & cont < 100 )
    cont = cont + 1;
	x=(a+b)*0.5;

    if f(a)*f(x) < 0
        b=x;
    else
        a=x;
    end
	
    a;
    b;
	erro=abs(f(x));
end
printf("\n A raiz de f(x)=x.ln(x)-1 eh %.10f com criterio de parada |f(x)| <= %.10f com %d iteracoes\n\n",x,erro,cont);