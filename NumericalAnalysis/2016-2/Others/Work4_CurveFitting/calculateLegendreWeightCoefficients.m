

%
% Calculo dos valores de t(m,k) para m = 1 até m
%
%Calcula os coeficientes dos polinomios de Legendre e suas raizes, p/ todo m
%recorrencia a partir de P(n-1) e P(n-2) conhecidos
%
function t = calculateLegendreWeightCoefficients( m )

    % (n)*P(n)-(2n-1)*x*P(n-1)+(n-1)*P(n-2)=0
    % P(n)=((2n-1)*x*P(n-1)-(n-1)*P(n-2))/n
    %Matriz de coeficientes a(n,...): n=1:m
    % a(n,1)=(1-n)/n*a(n-2,1);
    % for k=2:n-1 a(n,k)=(1-n)/n*a(n-2,k)+(2*n-1)/n*a(n-1,k-1);
    % a(n,n )=(2*n-1)/n*a(n-1,n-1);
    % a(n,n+1)=(2*n-1)/n*a(n-1,n );

    %t(m,k) = Zeros dos Polinomios de Legendre de grau m
    t=zeros(m, m);

    %matriz de coeficientes dos polinomios de Legendre a(n,...)
    a=zeros(m+1, m+1);
    n=1;

    a(n,1)=0;

    %p1=0+1x
    a(n,2)=1;
    aux   = sort(transpose(roots(fliplr(a(n,1:n+1)))));

    for k=1:n

        t(n,k)=aux(k);

    end

    n=2; a(n,1)=-1/2; a(n,2)=0; a(n,3)=3/2; %p2=(-1+0x+3x^2)/2
    aux = sort(transpose(roots(fliplr(a(n,1:n+1)))));

    for k=1:n

        t(n,k)=aux(k);

    end

    for n=3:m %P(n)

        a(n,1)=(1-n)/n*a(n-2,1);

        for k=2:n-1

            a(n,k)=(1-n)/n*a(n-2,k)+(2*n-1)/n*a(n-1,k-1);

        end

        a(n,n ) =(2*n-1)/n*a(n-1,n-1);
        a(n,n+1)=(2*n-1)/n*a(n-1,n );

        aux = sort(transpose(roots(fliplr(a(n,1:n+1)))));

        for k=1:n

            t(n,k)=aux(k);

        end

    end

end



