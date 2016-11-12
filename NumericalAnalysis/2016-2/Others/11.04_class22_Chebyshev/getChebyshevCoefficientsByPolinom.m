
#
# Evaluate the t variable at the k'th Chebyshev Polynom analytically calculated.
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
#
# Efficient Computation of Chebyshev Polynomials in Computer Algebra
# http://www.mathematik.uni-kassel.de/koepf/cheby.pdf
#
# Mathematics 4330/5344–#5 Approximation of Functions
# http://texas.math.ttu.edu/~gilliam/ttu/m4330/m4330_5.pdf
#
# Are there functions that remember values they have found on Octave?
# http://stackoverflow.com/questions/40445316/are-there-functions-that-remember-values-they-have-found-on-octave
#
# @param k, the k'th Chebyshev Polynom index to be used
# @param t, the value to evaluate at the k'th Chebyshev Polynom
#
function result = getChebyshevCoefficientsByPolinom( k, t )

    polynom = getChebyshevCoefficientsPolinom( k );
    result  = polyval( polynom, t );

end


# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )



