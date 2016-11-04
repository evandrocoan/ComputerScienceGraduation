function [ x, operacoes, currentError ] = gaussSeidel( fator, n )
    
    for i = 1 : n
        
        xi( i ) = 1;
        
    end
    
    x = xi;
    
    aux       = ( 1 - fator );
    operacoes = 0;
    
    currentStep  = 0;
    maximumSteps = 1000;
    
    currentError = 1;
    desiredError = 1e-4;
    
    while( ( currentStep < maximumSteps ) && ( currentError > desiredError ) )
        
        currentStep = currentStep + 1;
        
        i = 1;
        x( i ) = x( i ) * aux + fator * ( ( - x( i + 1 ) + 450 ) / 3 );
        operacoes = operacoes + 2;
        
        for i = 2 : n / 2
            
            x( i ) = x( i ) * aux + fator * ( ( 100 + x( i + 1 ) + x( i + n / 2 ) + 20 * x( i - 1 ) ) / 50 );
            operacoes = operacoes + 3;
            
        end
        
        for i = n / 2 + 1 : n - 1
            
            x( i ) = x( i ) * aux + fator * ( ( 200 + 11 * x( i - n / 2 ) + 3 * x( i - 1 ) + x( i + 1 ) ) / 60 ); 
            operacoes = operacoes + 4;
            
        end
        
        i = n;
        x( i ) = x( i ) * aux + fator * ( ( 300 + 3 * x( i - 1 ) ) / 10 );
        operacoes = operacoes + 3;
        
        errors       = x .- x;
        currentError = 0;
        
        for i = 1 : n
            
            currentError = currentError + abs( errors( i ) );
            
        end
        
        currentError;
        xi = x;
        
    end
    
end


















