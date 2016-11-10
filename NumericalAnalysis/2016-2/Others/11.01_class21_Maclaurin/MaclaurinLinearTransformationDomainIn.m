

# Linear transformation to convert the [a, b] domain to [-1, 1] domain.
# We may call it as `t(x)`.
#
function t = MaclaurinLinearTransformationDomainIn( x, a, b )

    t = ( 2*x - (b+a) ) / ( b-a );

end







