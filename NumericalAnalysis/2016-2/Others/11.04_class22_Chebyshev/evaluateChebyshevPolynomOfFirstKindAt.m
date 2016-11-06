
# 
# Evaluate the t variable at the k'th Chebyshev Polynom
# 
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
# 
# @param k, the k'th Chebyshev Polynom
# @param t, the value to evaluate at the k'th Chebyshev Polynom
# 

function value = evaluateChebyshevPolynomOfFirstKindAt( k, t )
    
    switch( k )
        
        case 0
            value = T0( t );
            
        case 1
            value = T1( t );
            
        case 2
            value = T2( t );
            
        case 3
            value = T3( t );
            
        case 4
            value = T4( t );
            
        case 5
            value = T5( t );
            
        case 6
            value = T6( t );
            
        case 7
            value = T7( t );
            
        case 8
            value = T8( t );
            
        case 9
            value = T9( t );
            
         otherwise
            value = evaluateChebyshevPolynomOfFirstKind( k, t );
            
    end
    
end

# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )


function result = evaluateChebyshevPolynomOfFirstKind( k, t )

    if k == 0
        
        result = 1;
        
    elseif k == 1
        
        result =  t;
        
    elseif mod( k, 2 ) == 0
        
        result = 2*evaluateChebyshevPolynomOfFirstKind( k/2, t )^2 - 1;
        
    else
        
        result = 2*evaluateChebyshevPolynomOfFirstKind( (k-1) / 2, t )*evaluateChebyshevPolynomOfFirstKind( (k+1) / 2, t ) - t;
        
    end
    
end




value = 0.6;

T0_correct = T0( value )
T0_calcula = evaluateChebyshevPolynomOfFirstKind( 0, value )
printf( '\n' )

T1_correct = T1( value )
T1_calcula = evaluateChebyshevPolynomOfFirstKind( 1, value )
printf( '\n' )

T2_correct = T2( value )
T2_calcula = evaluateChebyshevPolynomOfFirstKind( 2, value )
printf( '\n' )

T3_correct = T3( value )
T3_calcula = evaluateChebyshevPolynomOfFirstKind( 3, value )
printf( '\n' )

T4_correct = T4( value )
T4_calcula = evaluateChebyshevPolynomOfFirstKind( 4, value )
printf( '\n' )

T5_correct = T5( value )
T5_calcula = evaluateChebyshevPolynomOfFirstKind( 5, value )
printf( '\n' )

T6_correct = T6( value )
T6_calcula = evaluateChebyshevPolynomOfFirstKind( 6, value )
printf( '\n' )

T7_correct = T7( value )
T7_calcula = evaluateChebyshevPolynomOfFirstKind( 7, value )
printf( '\n' )

T8_correct = T8( value )
T8_calcula = evaluateChebyshevPolynomOfFirstKind( 8, value )
printf( '\n' )

T9_correct = T9( value )
T9_calcula = evaluateChebyshevPolynomOfFirstKind( 9, value )
printf( '\n' )



