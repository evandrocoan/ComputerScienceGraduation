function [ x, operacoes, currentError ] = jabob( fator, n )
    
    for i = 1 : n
        
        xi( i ) = 1;
        
    end
    
    aux       = ( 1 - fator );
    operacoes = 0;
    
    currentStep  = 0;
    maximumSteps = 1000;
    
    currentError = 1;
    desiredError = 1e-4;
    
    while( ( currentStep < maximumSteps ) && ( currentError > desiredError ) )
        
        currentStep = currentStep + 1;
        
        i = 1;
        x( i ) = xi( i ) * aux + fator * ( ( - xi( i + 1 ) + 450 ) / 3 );
        operacoes = operacoes + 2;
        
        for i = 2 : n / 2
            
            x( i ) = xi( i ) * aux + fator * ( ( 100 + xi( i + 1 ) + xi( i + n / 2 ) + 20 * xi( i - 1 ) ) / 50 );
            operacoes = operacoes + 3;
            
        end
        
        for i = n / 2 + 1 : n - 1
            
            x( i ) = xi( i ) * aux + fator * ( ( 200 + 11 * xi( i - n / 2 ) + 3 * xi( i - 1 ) + xi( i + 1 ) ) / 60 ); 
            operacoes = operacoes + 4;
            
        end
        
        i = n;
        x( i ) = xi( i ) * aux + fator * ( ( 300 + 3 * xi( i - 1 ) ) / 10 );
        operacoes = operacoes + 3;
        
        errors       = xi .- x;
        currentError = 0;
        
        for i = 1 : n
            
            currentError = currentError + abs( errors( i ) );
            
        end
        
        currentError;
        xi = x;
        
    end
    
end


















