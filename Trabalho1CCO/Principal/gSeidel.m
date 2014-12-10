function x = gSeidel(errMax, numIt, fat)
	xi = [ 0 0 0 0 0 0 0 0 0 0];
	x  = xi;
	
	passo = 0;
	err = 1;
	
	while ( ( err > errMax ) && ( passo < numIt ) )
		passo = passo + 1;
 								
		x(1) = ( 1 - fat ) * xi(1) + fat * (4-x(2)-1.5*x(3))/2.5;
		x(2) = ( 1 - fat ) * xi(2) + fat * (-3-0.51*x(3)-0.1*x(5))/0.52;
	    	x(3) = ( 1 - fat ) * xi(3) + fat * (1-0.9*x(1)-x(2)-x(4))/2.9;	    	
   	      x(4) = ( 1 - fat ) * xi(4) + fat * (-1-x(2)-0.2*x(3)-x(5))/2.2;     	
  	      x(5) = ( 1 - fat ) * xi(5) + fat * (-1-x(1)-2.0*x(4)-x(6))/4.0;     	
  		x(6) = ( 1 - fat ) * xi(6) + fat * (-1*x(2)+2.0*x(5)+x(7))/4.0;    	 	
	   	x(7) = ( 1 - fat ) * xi(7) + fat * (-1-x(1)-2.0*x(6)-x(8))/4.0;  	    	
	   	x(8) = ( 1 - fat ) * xi(8) + fat * (1+x(2)-x(7)-x(9))/3.0;     			
	   	x(9) = ( 1 - fat ) * xi(9) + fat * (3-x(3)+x(8)+x(10))/-3.0;      		
	   	x(10)= ( 1 - fat ) * xi(10) + fat * (-2+x(4)-x(9))/2.0;
		
		err = max( abs( x .- xi ) );
		xi = x;
	end
end
