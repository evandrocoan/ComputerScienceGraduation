

#
# For the Simpson's degree 2, the n intervals must to be even. Here we use a polynom of degree 2,
# instead of a polynom with degree 1 ( trapeze A=(B+b/2), greater base + smaller base ).
# n             = even;
# h             = (b-a)/n;
# intevalsCount = numel( h );
#
# The each one of the Simpson's Area is named by its middle point. Example:
# x1, x2, x3, x4, x5, x6, x7
#     A2,     A4,     A6
#
# A2 = \integral_x1^x3 = h/3 * ( y(1) + 4*y(2) + y(3) )
# A4 = \integral_x1^x3 = h/3 * ( y(3) + 4*y(3) + y(4) )
# A6 = \integral_x1^x3 = h/3 * ( y(5) + 4*y(6) + y(7) )
#

function aproximateIntegral = problem1SimpsonNumericIntegralDegree2( n, a, b )

    # On Matlab not equals is ~=
    # if mod( n, 2 ) != 0
    if mod( n, 2 ) ~= 0

        n = n + 1

    end

    h = (b-a) / n;
    x = a : h : b;

    y = problem1Function( x );

    soma1 = 0;
    soma2 = 0;

    for i = 2 : 2 : n

        soma1 = soma1 + y( i );

    end

    for i = 3 : 2 : n - 1

        soma2 = soma2 + y( i );

    end

    aproximateIntegral = ( y(1) + 4*soma1 + 2*soma2 + y(n+1) ) * h / 3;

end


