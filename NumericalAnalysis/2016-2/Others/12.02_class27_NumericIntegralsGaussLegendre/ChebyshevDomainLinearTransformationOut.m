

# Linear transformation to convert the [-1, 1] domain to [a, b] domain.
#
# We may call it as `x(t)`. On this way, we apply the the approximation
# methods to the `f(x(t))`, were `x` belongs to the Domain [a, b].
#
function x = ChebyshevDomainLinearTransformationOut( t, a, b )

    x = ( (b-a)*t + (b+a) ) / 2;

end






