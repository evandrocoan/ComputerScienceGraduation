function [ result, operacoes ] = rmax( Matrix, line_count, solution, operacoes )
    
    Matrix;
    line_count;
    
    for current_line = 1 : line_count
        
        sum = 0;
        
        for current_column = 1 : line_count
            
            % solution( current_column )
            % Matrix( current_line, current_column )
            
            sum = sum + Matrix( current_line, current_column ) * solution( current_column );
            operacoes = operacoes + 1;
            
        end
        
        residues( current_line ) = abs( sum - Matrix( current_line, line_count + 1 ) );
        
    end
    
    result = max( residues );
    
end
