function MatrixResulting = pivotamento_parcial( Matrix, current_line )
    
    [ max_value, aimed_line ] = max( Matrix( :, current_line ) )
    
    if( aimed_line > current_line )
        
        temp_line                 = Matrix( current_line, : );
        Matrix( current_line, : ) = Matrix( aimed_line, : )
        Matrix( aimed_line, : )   = temp_line
        
    end
    
    MatrixResulting = Matrix;
    
end
