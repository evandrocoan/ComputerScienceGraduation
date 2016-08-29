% Assuming an matrix as 3x4, 2x4, etc.
function Solution = fgauss( Matrix, line_size, column_size )
	
	last_column_to_process = column_size - 1
	
	for current_column = 1 : last_column_to_process
		
		current_processing_step = current_column + 1
		Matrix                  = pivotamento_parcial( Matrix, current_column, column_size );
		
		for current_line = current_processing_step : line_size
			
			current_element                        = Matrix( current_line, current_column )
			aux                                    = current_element / Matrix( current_column, current_column )
			Matrix( current_line, current_column ) = 0
			
			for non_zero_column = current_processing_step : column_size
				
				current_element                         = Matrix( current_line, non_zero_column )
				Matrix( current_line, non_zero_column ) = current_element - aux * current_element
				
			end
			
		end
		
	end
	
	Solution = [ Matrix( :, column_size ) ];
	
end





