function x = jacob(errMax, numIt, fat)
	xi = [ 0 0 0 0 0 0 0 0 0 0];
	x  = [ 0 0 0 0 0 0 0 0 0 0];
	
	passo = 0;
	err = 1;
	
	while ( ( err > errMax ) && ( passo < numIt ) )
		passo = passo + 1;
 								
		x(1) = ( 1 - fat ) * xi(1) + fat * (4-xi(2)-1.5*xi(3))/2.5;
		x(2) = ( 1 - fat ) * xi(2) + fat * (-3-0.51*xi(3)-0.1*xi(5))/0.52;
	    	x(3) = ( 1 - fat ) * xi(3) + fat * (1-0.9*xi(1)-xi(2)-xi(4))/2.9;	    	
   	      x(4) = ( 1 - fat ) * xi(4) + fat * (-1-xi(2)-0.2*xi(3)-xi(5))/2.2;     	
  	      x(5) = ( 1 - fat ) * xi(5) + fat * (-1-xi(1)-2.0*xi(4)-xi(6))/4.0;     	
  		x(6) = ( 1 - fat ) * xi(6) + fat * (-1*xi(2)+2.0*xi(5)+xi(7))/4.0;    	 	
	   	x(7) = ( 1 - fat ) * xi(7) + fat * (-1-xi(1)-2.0*xi(6)-xi(8))/4.0;  	    	
	   	x(8) = ( 1 - fat ) * xi(8) + fat * (1+xi(2)-xi(7)-xi(9))/3.0;     			
	   	x(9) = ( 1 - fat ) * xi(9) + fat * (3-xi(3)+xi(8)+xi(10))/-3.0;      		
	   	x(10)= ( 1 - fat ) * xi(10) + fat * (-2+xi(4)-xi(9))/2.0;   	
		
		err = max( abs( x .- xi ) );
		xi = x;
	end
end
