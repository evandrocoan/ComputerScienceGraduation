function [a b]=fPade(npade,mpade,c)
%valido para n=n ou n=m+1
%calcular os b´s via sistema de eqs.
    k = npade - mpade;
    for i = 1:mpade
        for j = 1:i
            A(i,j)=c(k+i+j);
            A(j,i)=A(i,j);
        end
    A(i,mpade+1)=-c(npade+i+1);
    end
    A;
    aux = fgaussP(mpade, A); %b começa de 1, igual no texto
    b=fliplr(aux); %inverte os indices do vetor
%completandoo vetor b:
    b(mpade+1:npade)=0;
    b=[1 b]; %incluindo o 1º b, unitario
%calcular os As
    a(1)=c(1);
    for i=2:npade+1
        S = c(i);
        for j = 1:i-1
            S = S + b(j+1)*c(i-j);
        end
        a(i) = S;
    end
%R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
end