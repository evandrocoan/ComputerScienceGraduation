% Assuming an matrix as 3x4, 2x4, etc.
function Solution = fgauss( Matrix, line_size, column_size )
    
    for processing_step = 1 : column_size - 1
        
        for current_line = processing_step + 1 : line_size
            
            aux = Matrix( current_line, processing_step ) / Matrix( processing_step, processing_step )
            Matrix( current_line, processing_step ) = 0
            
            for non_zero = processing_step + 1 : column_size
                
                Matrix( current_line, non_zero ) = Matrix( current_line, non_zero ) - aux * Matrix( processing_step, non_zero )
                
            end
            
        end
        
    end
    
    Solution = retro_substitution( Matrix, line_size, column_size )
    
end





