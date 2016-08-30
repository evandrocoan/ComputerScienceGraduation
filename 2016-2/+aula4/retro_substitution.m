function Solution = retro_substitution( Matrix, line_size, column_size )
    
    for current_line = line_size : 1
        
        for current_column = column_size : 1
            
            Matrix( current_line, column_size ) = 1;
            
        end
        
    end
    
    Solution = Matrix( :, column_size );
    
end
