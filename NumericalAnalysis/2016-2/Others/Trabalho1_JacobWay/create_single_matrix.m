function Matrix = create_single_matrix( n )

    i = 1;
    Matrix( i, i )     = 3;
    Matrix( i, i + 1 ) = 1;
    Matrix( i, n + 1 ) = 450;
    
    for i = 2 : n / 2
        Matrix( i, i - 1 )     = 20;
        Matrix( i, i )         = 50;
        Matrix( i, i + 1 )     = 1;
        Matrix( i, i + n / 2 ) = 1;
        Matrix( i, n + 1 )     = 100;
    end
    
    for i = n / 2 + 1 : n - 1
        Matrix( i, i - n / 2 ) = 11;
        Matrix( i, i - 1 )     = 3;
        Matrix( i, i )         = 60;
        Matrix( i, i + 1 )     = 1;
        Matrix( i, n + 1 )     = 200;
    end
    
    i = n;
    Matrix( i, i - 1 ) = 3;
    Matrix( i, i )     = 10;
    Matrix( i, n + 1 ) = 300;
    
end
