function x=fSqrtNR(C, erroPropNR)
      %sqrt(C)=x=>? ; f(x)=x^2-C; f'(x)=2*x
      %x=xi-f(xi)/df(xi); x=x-(x^2-C)/(2*x);  x=0.5*(x*x+C)/x;
      if(C>1) x=1+eps;
      else x=0+eps;
      end
      cont=0;erro=1;
      while(erro>erroPropNR && cont<100)
         cont++;
         xi=x;
         x=0.5*(x*x+C)*fReciproco(x); %*1/x;
         erro=abs(x-xi);
      end
end
