function chebyshev_test01 ( )

%*****************************************************************************80
%
%% CHEBYSHEV_TEST01 tests CHEBYSHEV_COEFFICIENTS and CHEBYSHEV_INTERPOLANT.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 September 2011
%
%  Author:
%
%    John Burkardt
%
  printf( '\n' );
  printf( 'CHEBYSHEV_TEST01\n' );
  printf( '  CHEBYSHEV_COEFFICIENTS computes the coefficients of the\n' );
  printf( '  Chebyshev interpolant.\n' );
  printf( '  CHEBYSHEV_INTERPOLANT evaluates the interpolant.\n' );

  n = 5;
  a = -1.0;
  b = +1.0;

  c = chebyshev_coefficients ( a, b, n, @f1 );

  x = chebyshev_zeros ( n );
  x = 0.5 * ( a + b ) + x * 0.5 * ( b - a );

  fx = f1 ( x );
  m = n;
  fc = chebyshev_interpolant ( a, b, n, c, m, x );

  printf( '\n' );
  printf( '  F(X) is a trig function:\n' );
  printf( '\n' );
  printf( '      X           C(I)        F(X)       C(F)(X)\n' );
  printf( '\n' );
  for i = 1 : n
    printf( '  %10f  %10f  %10f  %10f\n', x(i), c(i), fx(i), fc(i) );
  end
%
%  Try a variant interval.
%
  n = 5;
  a = 0.0;
  b = +3.0;

  c = chebyshev_coefficients ( a, b, n, @f1 );

  x = chebyshev_zeros ( n );
  x = 0.5 * ( a + b ) + x * 0.5 * ( b - a );

  fx = f1 ( x );
  m = n;
  fc = chebyshev_interpolant ( a, b, n, c, m, x );

  printf( '\n' );
  printf( '  Consider the same F(X), but now over [0,3]:\n' );
  printf( '\n' );
  printf( '      X           C(I)        F(X)       C(F)(X)\n' );
  printf( '\n' );
  for i = 1 : n
    printf( '  %10f  %10f  %10f  %10f\n', x(i), c(i), fx(i), fc(i) );
  end
%
%  Try a higher order.
%
  n = 10;
  a = -1.0;
  b = +1.0;

  c = chebyshev_coefficients ( a, b, n, @f1 );

  x = chebyshev_zeros ( n );
  x = 0.5 * ( a + b ) + x * 0.5 * ( b - a );

  fx = f1 ( x );
  m = n;
  fc = chebyshev_interpolant ( a, b, n, c, m, x );

  printf( '\n' );
  printf( '  Consider the same F(X), but now with higher order:\n' );
  printf( '\n' );
  printf( '      X           C(I)        F(X)       C(F)(X)\n' );
  printf( '\n' );
  for i = 1 : n
    printf( '  %10f  %10f  %10f  %10f\n', x(i), c(i), fx(i), fc(i) );
  end
%
%  Try a polynomial.
%
  n = 10;
  a = -1.0;
  b = +1.0;

  c = chebyshev_coefficients ( a, b, n, @f3 );

  x = chebyshev_zeros ( n );
  x = 0.5 * ( a + b ) + x * 0.5 * ( b - a );

  fx = f3 ( x );
  m = n;
  fc = chebyshev_interpolant ( a, b, n, c, m, x );

  printf( '\n' );
  printf( '  F(X) is a degree 4 polynomial:\n' );
  printf( '\n' );
  printf( '      X           C(I)        F(X)       C(F)(X)\n' );
  printf( '\n' );
  for i = 1 : n
    printf( '  %10f  %10f  %10f  %10f\n', x(i), c(i), fx(i), fc(i) );
  end
%
%  Try a function with decaying behavior.
%
  n = 10;
  a = -1.0;
  b = +1.0;

  c = chebyshev_coefficients ( a, b, n, @f2 );

  x = chebyshev_zeros ( n );
  x = 0.5 * ( a + b ) + x * 0.5 * ( b - a );

  fx = f2 ( x );
  m = n;
  fc = chebyshev_interpolant ( a, b, n, c, m, x );

  printf( '\n' );
  printf( '  The polynomial approximation to F(X) decays:\n' );
  printf( '\n' );
  printf( '      X           C(I)        F(X)       C(F)(X)\n' );
  printf( '\n' );
  for i = 1 : n
    printf( '  %10f  %10f  %10f  %10f\n', x(i), c(i), fx(i), fc(i) );
  end

  return
end

chebyshev_test01
