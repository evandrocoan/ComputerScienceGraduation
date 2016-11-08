

#
# Function: log( x )
# For the Domain [-1, 1]
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*z^1) / 1! + (f''( 0 )*z^2) / 2! + ... + (f^n'( 0 )*z^n) / n!
#
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*z^1) / 1!
#                        + (f''( 0 )*z^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*z^n) / n!
#
% function coef = calculatePadeCoefficients( n, m, coefMaclaurinParaPade )
function [a b] = calculatePadeCoefficients( npade, mpade, c )
    
    %valido para n=n ou n=m+1
    %calcular os b's via sistema de eqs.
    k = npade - mpade;
    
    for i = 1:mpade
        
        for j = 1:i
            
            A(i,j)=c(k+i+j);
            A(j,i)=A(i,j);
            
        end
        
        A(i,mpade+1)=-c(npade+i+1);
        
    end
    A;
    
    %b começa de 1, igual no texto
    aux = fgauss(mpade, A); 
    
    %inverte os indices do vetor
    b=fliplr(aux);
    
    %completandoo vetor b:
    b(mpade+1:npade)=0;
    
    %incluindo o 1o b, unitario
    b=[1 b]; 
    
    %calcular os As
    a(1)=c(1);
    
    for i=2:npade+1
        
        S = c(i);
        
        for j = 1:i-1
        
            S = S + b(j+1)*c(i-j);
        
        end
    
        a(i) = S;
    
    end
end


%R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)









