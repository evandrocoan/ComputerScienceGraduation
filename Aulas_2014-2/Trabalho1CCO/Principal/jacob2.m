function op = jacob2(errMax, numIt, fat)
	op = 0;
	
	xi = [ 0 0 0 0 0 0 0 0 0 0];
	x  = [ 0 0 0 0 0 0 0 0 0 0];
	
	passo = 0;
	err = 1;
	
	while ( ( err > errMax ) && ( passo < numIt ) )
		passo = passo + 1;
 								
		x(1) = ( 1 - fat ) * xi(1) + fat * (4-(xi(2)+1.5*xi(3)))/2.5;
		op = op + 8;
		x(2) = ( 1 - fat ) * xi(1) + fat * (-3-(0.51*xi(3) + 0.1*xi(5)))/0.52;
		op = op + 9;
	    	x(3) = ( 1 - fat ) * xi(1) + fat * (1-(0.9*xi(1) + xi(2) + xi(4)))/2.9;
	    	op = op + 9;
   	      x(4) = ( 1 - fat ) * xi(1) + fat * (-1-(xi(2) + 0.2*xi(3) + xi(5)))/2.2;     	
   	      op = op + 9;
  	      x(5) = ( 1 - fat ) * xi(1) + fat * (-1-(xi(1) + 2.0*xi(4) + xi(6)))/4.0;     	
  	      op = op + 9;
  		x(6) = ( 1 - fat ) * xi(1) + fat * (-(xi(2)-2.0*xi(5)-xi(7)))/4.0;
  		op = op + 8;
	   	x(7) = ( 1 - fat ) * xi(1) + fat * (-1-(xi(1)+2.0*xi(6)+xi(8)))/4.0;
	   	op = op + 9;
	   	x(8) = ( 1 - fat ) * xi(1) + fat * (1-(-xi(2)+xi(7)+xi(9)))/3.0;
	   	op = op + 8;
	   	x(9) = ( 1 - fat ) * xi(1) + fat * (3-(xi(3)-xi(8)-xi(10)))/-3.0;      		
	   	op = op + 8;
	   	x(10)= ( 1 - fat ) * xi(1) + fat * (-2-(xi(4)+xi(9)))/2.0;
	   	op = op + 7;
	   	
		err = max( abs( x .- xi ) );
		op = op + 1;
	   	   	
		xi = x;
	end
end
