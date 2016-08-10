function x=fReciproco(C)
	%1/C=x=>? ; f(x)=C-1/x; f'(x)=-1/(x*x)
	%x=xi-f(xi)/df(xi)  ; x=x-(C-1/x)/(1/(x*x))  ;  x=x*(2-x*C);
	if(C>1) x=0+eps;
	else x=1+eps;
	end
	cont=0;erro=1;
	while(erro>1.e-16 && cont<100)
	   cont++;
	   xi=x;
	   x=x*(2-x*C);
	   erro=abs(x-xi);
	end
end
