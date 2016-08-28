function aula3_fgauss( Matrix )
	
	
	column_size = size( Matrix, 1)
	
	% TODO
	for current_processing_step = 1 : column_size - 1
		
		for current_line = current_processing_step + 1 : column_size
			
			aux = Matrix( current_line, current_processing_step ) / Matrix( current_processing_step, current_processing_step )
			Matrix( current_line, : ) = Matrix( current_line, : ) - aux * Matrix( current_processing_step, : )
			
		end
		
	end
	
end





