function print_solution( solucao, n )
    output_precision(30)
    for i = 1 : n;
        
        printf( 'x(%2d) = %17f, ', i, solucao( i ) );
        
        if( mod( i, 5 ) == 0 )
            
            printf( '\n');
            
        end
        
    end
    
    printf( '\n' );
    
end
