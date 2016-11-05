function chebyshev_test ( )

%*****************************************************************************80
%
%% CHEBYSHEV_TEST tests the CHEBYSHEV library.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 September 2011
%
%  Author:
%
%    John Burkardt
%
  timestamp ( );
  printf( '\n' );
  printf( 'CHEBYSHEV_TEST\n' );
  printf( '  MATLAB version.\n' );
  printf( '  Test the CHEBYSHEV library.\n' );

  chebyshev_test01 ( );
%
%  Terminate.
%
  printf( '\n' );
  printf( 'CHEBYSHEV_TEST\n' );
  printf( '  Normal end of execution.\n' );
  printf( '\n' );
  timestamp ( );

  return
end


chebyshev_test
