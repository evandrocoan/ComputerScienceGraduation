

b(1,1).vector = @sin;
b(1,2).vector = @cos;
b(2,1).vector = @sec;
b(2,2).vector = @csc;

b
b.vector

printf( "\n\nCalling each element:\n" )
b(1,1).vector
b(1,2).vector
b(2,1).vector
b(2,2).vector

printf( "\nCalculatin the sin of 1:\n" )
b(1,1).vector(1)




function x = f( x )

    x = x^3/3 + log( cos(x) - 1 )/2 - log( cos(x) + 1 )/2

end

f(2)


#
# How do I declare a symbolic matrix in Octave?
#
# http://stackoverflow.com/questions/2472486/how-do-i-declare-a-symbolic-matrix-in-octave/40824167#40824167
# http://stackoverflow.com/a/40824167/4934640
#
# octave> pkg install -forge symbolic
#
pkg load symbolic;

b(1,1).vector = sym("a");
b(1,2).vector = sym("b");
b(2,1).vector = sym("c");
b(2,2).vector = sym("d");

b
b.vector

printf( "\n\nCalling each element:\n" )
b(1,1).vector
b(1,2).vector
b(2,1).vector
b(2,2).vector



