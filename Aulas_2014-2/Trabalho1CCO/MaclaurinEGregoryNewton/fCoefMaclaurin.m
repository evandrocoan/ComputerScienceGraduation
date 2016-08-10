function cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b )
    %tB = 0; ( Maclaurin )
    cMaclaurin(1) = log( 0.5 * (b - a) * 0 + .5 * (b + a) );
    for i = 2 : nMaclaurin + 1
    	cMaclaurin(i) =  (-1)^i * ( ( ( b - a  ) / ( ( b + a ) ) )^(i - 1) ) / ( i - 1 );
    end
end