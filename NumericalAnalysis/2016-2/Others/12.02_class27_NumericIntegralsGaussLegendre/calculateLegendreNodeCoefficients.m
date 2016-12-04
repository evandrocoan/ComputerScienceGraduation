

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

        k

        for i=1:k

            for j=1:k

                A(i,j)=t(k,j)^(i-1);

            end

        end

        b(1:k)=0;

        for i=1:2:k

            b(i)=(2/i);

        end

        A = [A transpose(b)]

        if k == 1

            C( k, k ) = A( 2 );

        else

            # aux = A;
            aux = fpivotacaototal(1,A)

            for j=1:k

                C(k, j)=aux( 1, j )

            end %armazena na forma de matriz

        end

    end %m

end


% escolha e localização do maior modulo da matriz, a partir da linha k: Amax, imax e jmax
function A=fpivotacaototal(k,A)

    linesCount   = size( A, 1 );
    columnsCount = size( A, 2 );

    Amax = abs(A(k,k));
    imax = k;
    jmax = k;

    for i=k:linesCount

        for j=k:linesCount

        if(abs(A(i,j))>Amax)

            Amax = abs(A(i,j));
            imax = i;
            jmax = j;

            end %if

        end %for j

    end %for i

    % troca de linhas, entre a linha i=k e a linha i=imax
    Aux=A(k,:);
    A(k,:)=A(imax,:);
    A(imax,:)=Aux;

    % troca de colunas, entre a coluna j=k e a coluna j=jmax
    Aux=A(:,k);
    A(:,k)=A(:,jmax);
    A(:,jmax)=Aux;

end %function

A = [ 1 2 ; 2 1];
A = [  1.000000000000000   1.000000000000000   2.000000000000000
      -0.577350269189626   0.577350269189626   0.000000000000000 ]

fpivotacaototal(1,A)

