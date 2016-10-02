function [ Solution, operacoes ] = fgauss_sem_pivotacao( Matrix, line_count, column_count )
    
    operacoes = 0;
    
    for processing_step = 1 : line_count - 1
        
        processing_step;
        
        for current_line = processing_step + 1 : line_count
            
            current_line;
            dividendo = Matrix( current_line, processing_step );
            divisor   = Matrix( processing_step, processing_step );
            aux       = dividendo / divisor;
            operacoes++;
            
            Matrix( current_line, processing_step ) = 0;
            
            for non_zero = processing_step + 1 : column_count
                
                non_zero;
                Matrix( current_line, non_zero ) = Matrix( current_line, non_zero ) - aux * Matrix( processing_step, non_zero );
                operacoes++;
                
            end
            
        end
        
    end
    
    % Calculates the first term, 4x=5 --> x = 5/4
    Matrix( line_count, column_count ) = Matrix( line_count, column_count ) / Matrix( line_count, column_count - 1 );
    current_line                       = line_count - 1;
    operacoes++;
    
    % Calculate the remaining terms.
    while( current_line > 0 )
        
        sum = 0;
        
        for non_zero = column_count - 1: -1: current_line + 1
            
            non_zero;
            multiplado    = Matrix( current_line, non_zero );
            multiplicador = Matrix( non_zero, column_count );
            sum           = sum + multiplado * multiplicador;
            operacoes++;
            
        end
        
        Matrix( current_line, column_count ) = (  ( ( ( -1 * sum ) + Matrix( current_line, column_count ) ) / Matrix( current_line, current_line ) ) );
        current_line = current_line - 1;
        operacoes++;
        
    end
    
    Solution = Matrix( :, column_count )';
    
end
