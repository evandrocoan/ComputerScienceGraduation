function x = fGaussTrid( n, t, r, d, b )
    
    A = [ t, r, d, b ]
    
    #     1 2 3 4 5 6 7
    #    ______________
    # 1 | r d 0 0 0 0 b
    # 2 | t r d 0 0 0 b
    # 3 | 0 t r d 0 0 b
    # 4 | 0 0 t r d 0 b
    # 5 | 0 0 0 t r d b
    # 6 | 0 0 0 0 t r b
    
    column_count = 4;
    line_count   = n;
    
    # Navega/anda pela diagonal principal
    for i = 1 : line_count - 1
        
        printf( '\n\n\nStarting the %dº iteration...\n', i )
        
        # Navega/anda pelas linhas abaixo da diagonal principal
        # 
        # for M( 1, 1 ), mapping should return 2
        # for M( 2, 2 ), mapping should return 2
        # ...
        for j = i + 1 : i + 1
            
            j
            mapped_ji = mapping( j, i, line_count, column_count )
            mapped_ii = mapping( i, i, line_count, column_count )
            
            dividendo = A( j, mapped_ji )
            divisor   = A( i, mapped_ii )
            aux       = dividendo / divisor
            
            # A =                 M =      1  2  3  4  5  6 ... 51
            #    0   2  -1  -1           _________________________
            #   -1   3  -1   1        1 |  2 -1  0  0  0  0 ... -1
            #   -1   3  -1   1        2 | -1  3 -1  0  0  0 ...  1
            #   -1   3  -1   1        3 |  0 -1  3 -1  0  0 ...  1
            #   -1   3  -1   1        4 |  0  0 -1  3 -1  0 ...  1
            #   -1   3  -1   1        5 |  0  0  0 -1  3 -1 ...  1
            #   -1   3  -1   1        6 |  0  0  0  0 -1  3 ...  1
            #   -1   3  -1   1        7 |  0  0  0  0  0 -1 ...  1
            #  ...                  ... |
            
            # Navega/anda pelas colunas à esquerda da diagonal principal
            for k = i + 1 : i + 1
                
                k
                mapped_ik = mapping( i, k, line_count, column_count )
                mapped_jk = mapping( j, k, line_count, column_count )
                
                A( j, mapped_jk ) = A( j, mapped_jk ) - aux * A( i, mapped_ik );
                
            end
            
            i
            mapped_ji = mapping( j, i, line_count, column_count )
            
            A( j, mapped_ji    ) = 0;
            A( j, column_count ) = A( j, column_count ) - aux * A( i, column_count );
            
        end
        
    end
    
    printf( '\n' )
    A
    
    x = retro_substitution( A, line_count, column_count );
    printf( '\n' )
    
end

function solution = retro_substitution( A, line_count, column_count )
    
    printf( '\n\n\n\n\n\nStarting the retro_substitution...\n\n' )
    
    % Calculates the first term, 4x=5 --> x = 5/4
    mapped_line_column_count      = mapping( line_count, line_count, line_count, column_count )
    A( line_count, column_count ) = A( line_count, column_count ) / A( line_count, mapped_line_column_count );
    
    % Calculate the remaining terms.
    for i = line_count - 1 : -1: 1
        
        printf( '\n\n\nStarting the %dº iteration...\n', i )
        sum = 0
        
        for j = i + 1: -1: i + 1
            
            j
            mapped_ij = mapping( i, j, line_count, column_count )
            
            multiplicado  = A( i, mapped_ij    )
            multiplicador = A( j, column_count )
            sum           = sum + multiplicado * multiplicador
            
        end
        
        mapped_ii = mapping( i, i, line_count, column_count )
        
        A( i, column_count ) = ( ( -1 * sum ) + A( i, column_count ) ) / A( i, mapped_ii );
        i                    = i - 1
        
    end
    
    solution = A( :, column_count );
    
end





