% Assuming an matrix as 3x4, 2x4, etc.
function Solution = fgauss( Matrix, line_count, column_count )
    
    for processing_step = 1 : line_count - 1
        
        processing_step
        #Matrix = pivotamento_parcial( Matrix, processing_step )
        
        for current_line = processing_step + 1 : line_count
            
            current_line
            dividendo = Matrix( current_line, processing_step )
            divisor   = Matrix( processing_step, processing_step )
            aux       = dividendo / divisor
            
            Matrix( current_line, processing_step ) = 0
            
            for non_zero = processing_step + 1 : column_count
                
                non_zero
                Matrix( current_line, non_zero ) = Matrix( current_line, non_zero ) - aux * Matrix( processing_step, non_zero )
                
            end
            
        end
        
    end
    
    Solution = retro_substitution( Matrix, line_count, column_count );
    
end





