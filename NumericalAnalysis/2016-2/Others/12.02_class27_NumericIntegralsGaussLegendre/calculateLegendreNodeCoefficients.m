

%
% Calculo dos valores de C(m,k) para m = 1 até m
%
function C = calculateLegendreNodeCoefficients( m, t )

    %dados valores de t(m,k):
    %A=[
    % 1 1 1 ... 1 2/1;
    % tm1 tm2 tm3 ... tmm 0 ;
    % tm1² tm2² tm3² ... tmm² 2/3;
    % tm1³ tm2³ tm3³ ... tmm³ 0 ;
    % tm1⁴ tm2⁴ tm3⁴ ... tmm⁴ 2/5;
    % ...
    % tm1^(m-1) tm2^(m-1) tm3^(m-1) ... tmm^(m-1) 0 ;
    % ]

    C = zeros(m,m);

    for k=1:m

        k;

        for i=1:k

            for j=1:k

                A(i,j)=t(k,j)^(i-1);

            end

        end

        b(1:k)=0;

        for i=1:2:k

            b(i)=(2/i);

        end

        A = [A transpose(b)];

        # aux = A;
        # aux = fgausspivparcial(k, A);
        aux = fgauss( k, A );

        for j=1:k

            C(k, j)=aux( 1, j );

        end %armazena na forma de matriz

    end %m

end

