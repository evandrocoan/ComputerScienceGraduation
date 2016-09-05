function Solution = retro_substitution( Matrix, line_count, column_count )
    
    printf( '\n\n\n\n\n\nStarting the retro_substitution...\n\n' )
    % Calculates the first term
    % 4x=5 --> x = 5/4
    Matrix( line_count, column_count ) = Matrix( line_count, column_count ) / Matrix( line_count, column_count - 1 );
    
    current_line = line_count - 1
    
    % Calculate the remaining terms.
    while( current_line > 0 )
        
        sum = 0
        
        for non_zero = column_count - 1: -1: current_line + 1
            
            sum = sum + Matrix( current_line, non_zero ) * Matrix( non_zero, column_count )
            
        end
        
        Matrix( current_line, column_count ) = - sum / Matrix( current_line, current_line )
        current_line = current_line - 1
        
    end
    
    Solution = Matrix( :, column_count );
    
end
