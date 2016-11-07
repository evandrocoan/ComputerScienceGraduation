
#
# i = 1 : n
# fChebyshev( i ) = b0*T_( t(i) ) + b1*T_( t(i) ) + b2*T_( t(i) ) + b3*T_( t(i) ) + ...
# 


function x = T0( x )
    x = x.^0;
end
function x = T_()
    x = [ 1 ];
end


function x = T1( x )
    x = x;
end
function x = T_()
    x = [ 1, 0 ];
end


function x = T2( x )
    x = 2*x.^2 - 1;
end
function x = T_()
    x = [ 2, 0, -1 ];
end


function x = T3( x )
    x = 4*x.^3 - 3*x;
end
function x = T_()
    x = [ 4, 0, -3, 0 ];
end


function x = T4( x )
    x = 8*x.^4 - 8*x.^2 + 1;
end


function x = T_()
    x = [ 8, 0, -8, 0, 1 ];
end


function x = T5( x )
    x = 16*x.^5 - 20*x.^3 + 5*x;
end


function x = T_()
    x = [ 16, 0, -20, 0, 5, 0 ];
end


function x = T6( x )
    x = 32*x.^6 - 48*x.^4 + 18*x.^2 - 1;
end


function x = T_()
    x = [ 32, 0, -48, 0, 18, 0, -1 ];
end


function x = T7( x )
    x = 64*x.^7 - 112*x.^5 + 56*x.^3 - 7*x;
end


function x = T_()
    x = [ 64, 0, -112, 0, 56, 0, -7, 0 ];
end


function x = T8( x )
    x = 128*x.^8 - 256*x.^6 + 160*x.^4 - 32*x.^2 + 1;
end


function x = T_()
    x = [ 128, 0, -256, 0, 160, 0, -32, 0, 1 ];
end


function x = T9( x )
    x = 256*x.^9 - 576*x.^7 + 432*x.^5 - 120*x.^3 + 9*x;
end


function x = T_()
    x = [ 256, 0, -576, 0, 432, 0, -120, 0, 9, 0 ];
end




