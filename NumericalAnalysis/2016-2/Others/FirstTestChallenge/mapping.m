# Recebe coordenadas de uma matrix completa n x n, e calcula o índice da coluna correspondente
# em uma matrix da forma A = [ t, r, d, b ], onde t, r, d, b são vetores coluna de tamanho n.
# 
#     1 2 3 4 5 6 7
#    ______________
# 1 | 2 3 0 0 0 0 4
# 2 | 1 2 3 0 0 0 4
# 3 | 0 1 2 3 0 0 4
# 4 | 0 0 1 2 3 0 4
# 5 | 0 0 0 1 2 3 4
# 6 | 0 0 0 0 1 2 4
# 
# @param i               a linha atual da matrix quadrada de origem
# @param j               a coluna atual da matrix quadrada de origem
# @param line_count      o numero de linhas da matrix quadrada de origem
# @param column_count    o numero de colunas da matrix quadrada de origem
# 
# @return mapped_j       o novo índice da matrix retangular invertida de destino
#
function mapped_j = mapping( i, j, line_count, column_count )
    
    # j = 1, column_count = 3, mod = 0, + 1 = 1
    # j = 2, column_count = 3, mod = 1, + 1 = 2
    # j = 3, column_count = 3, mod = 2, + 1 = 3
    # j = 4, column_count = 3, mod = 0, + 1 = 1
    # mapped_j = mod( j - 1, column_count - 1 ) + 1
    
    # ( 1, 1 ) -> 2
    # ( 1, 2 ) -> 3
    # ( 1, 3 ) -> NaN columns 3..6
    # ( 2, 1 ) -> 1
    # ( 2, 2 ) -> 2
    # ( 2, 3 ) -> 3
    # ( 2, 4 ) -> NaN columns 4..6
    # ( 3, 1 ) -> NaN lines   1..1
    # ( 3, 2 ) -> 1
    # ( 3, 3 ) -> 2
    # ( 3, 4 ) -> 3
    # ( 3, 5 ) -> NaN columns 5..6
    # ( 4, 1 ) -> NaN lines   1..2
    # ( 4, 3 ) -> 1
    # ( 4, 4 ) -> 2
    # ( 4, 5 ) -> 3
    # ( 4, 6 ) -> NaN columns 6..6
    # ( 5, 1 ) -> NaN lines   1..3
    # ( 5, 4 ) -> 1
    # ( 5, 5 ) -> 2
    # ( 5, 6 ) -> 3
    # ( 6, 1 ) -> NaN lines   1..4
    # ( 6, 5 ) -> 1
    # ( 6, 6 ) -> 2
    
    # Chooses the correct line
    switch( i )
        
        case 1
            
            switch( j )
                
                case 1
                    
                    mapped_j =  2;
                    
                case 2
                    
                    mapped_j = 3;
                    
                otherwise
                    
                    mapped_j = 0;
                    
            end
            
        case line_count
            
            switch( j )
                
                case line_count - 1
                    
                    mapped_j =  1;
                    
                case line_count
                    
                    mapped_j = 2;
                    
                otherwise
                    
                    mapped_j = 0;
                    
            end
            
        otherwise
            
            switch( j - i )
                
                case -1
                    
                    mapped_j =  1;
                    
                case 0
                    
                    mapped_j = 2;    
                    
                case 1
                    
                    mapped_j = 3;
                    
                otherwise
                    
                    mapped_j = 0;
                    
            end
            
    end
    
end






