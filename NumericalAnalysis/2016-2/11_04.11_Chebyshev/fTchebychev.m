function b = fTchebychev(k)
%aproximacao de f(x)=ln(x) com x pertencente [1, 2]
%transformação de x para t no intervalo entre [-1, +1]
%x(t)=0.5*(b-a)*t+0.5*(b+a);
%f(x(t))=ln(0.5*t+1.5) com t pertencente a [-1, +1]

    m=1e1
    j=1:m; t=cos((2.*j.-1).*pi./(2.*m)); %m 'nós' de Tchebyschev
%T0=1;f(t)*1
    i=0;
%integrais de Gauss-Tchebyshev:
    soma=0;
    for j=1:m
        soma=soma+fx0(t(j))*(1);
    end
%y=pi/m*soma;
%b(1)=1/pi*y;
    b(i+1)=1/m*soma;
%T1=x; f(t)*t
    i=1;
    soma=0;
    for j=1:m
        soma=soma+fx0(t(j))*(t(j));
    end
    b(i+1)=2/m*soma;

%T2=(2t^2-1)
    i=2;
    soma=0;

    for j=1:m
        soma=soma+fx0(t(j))*(2*t(j)^2-1);
    end
    b(i+1)=2/m*soma;

%T3=(4t^3-3x)
    i=3;
    soma=0;
    for j=1:m
        soma=soma+fx0(t(j))*(4*t(j)^3-3*t(j));
    end
    b(i+1)=2/m*soma;
%T4=(8t^4-8t^2+1)
i=4;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(8*t(j)^4-8*t(j)^2+1);
end
b(i+1)=2/m*soma;
%T5=(16t^5-20t^3+5t)
i=5;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(16*t(j)^5-20*t(j)^3+5*t(j));
end
b(i+1)=2/m*soma;

%T6=(32t^6-48t^4+18t^2-1)
i=6;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(32*t(j)^6-48*t(j)^4+18*t(j)^2-1);
end
b(i+1)=2/m*soma;

i=7;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(64*t(j)^7-112*t(j)^5+56*t(j)^3-7*t(j));
end
b(i+1)=2/m*soma;

i=8;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(128*t(j)^8-256*t(j)^6+160*t(j)^4-32*t(j)^2+1);
end
b(i+1)=2/m*soma;

i=9;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(256*t(j)^9-576*t(j)^7+432*t(j)^5-120*t(j)^3+9*t(j));
end
b(i+1)=2/m*soma;

i=10;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(512*t(j)^10-1280*t(j)^8+1120*t(j)^6-400*t(j)^4+50*t(j)^2-1);
end
b(i+1)=2/m*soma;

i=11;
soma=0;
for j=1:m
soma=soma+fx0(t(j))*(1024*t(j)^11-2816*t(j)^9+2816*t(j)^7-1232*t(j)^5+220*t(j)^3-11*t(j));
end
b(i+1)=2/m*soma;


end