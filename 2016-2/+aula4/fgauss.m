% Assuming an matrix as 3x4, 2x4, etc.
function Solution = fgauss( Matrix, line_count, column_count )
    
    for processing_step = 1 : line_count - 1
        
        processing_step
        Matrix = pivotamento_parcial( Matrix, processing_step )
        
        for current_line = processing_step + 1 : line_count
            
            current_line
            aux = Matrix( current_line, processing_step ) / Matrix( processing_step, processing_step )
            
            for non_zero = processing_step + 1 : column_count
                
                non_zero
                Matrix( current_line, non_zero ) = Matrix( current_line, non_zero ) - aux * Matrix( processing_step, non_zero )
                
            end
            
            Matrix( current_line, processing_step ) = 0;
            
        end
        
    end
    
    Solution = retro_substitution( Matrix, line_count, column_count );
    
end





