ManualOperatedMatrix = [ pi       ,  e         ,  sqrt(2)       , 1; 
                         sqrt( 3 ),  sqrt( 10 ), -1 / sqrt( 3 ) , 2; 
                         e        , -sqrt( 2 ) ,  1 / sqrt( 10 ), 3; ]

OriginalInicialMatrix = ManualOperatedMatrix;


aux1 = ManualOperatedMatrix( 2, 1 ) / ManualOperatedMatrix( 1, 1 );
ManualOperatedMatrix( 2, : )= ManualOperatedMatrix( 2, : ) - aux1 * ManualOperatedMatrix( 1, : );

aux2 = ManualOperatedMatrix( 3, 1 ) / ManualOperatedMatrix( 1, 1 );
ManualOperatedMatrix( 3, : )=ManualOperatedMatrix( 3, : ) - aux2 * ManualOperatedMatrix( 1, : );

aux3 = ManualOperatedMatrix( 3,2 ) / ManualOperatedMatrix( 2,  2);
ManualOperatedMatrix( 3, : ) = ManualOperatedMatrix( 3, : ) - aux3 * ManualOperatedMatrix( 2, : );

ManualOperatedMatrix


aula3_fgauss( OriginalInicialMatrix )

OriginalInicialMatrix

