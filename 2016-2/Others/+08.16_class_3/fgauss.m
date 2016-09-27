function fgauss( Matrix )
	
	line_count = size( Matrix, 1)
	
	% TODO
	for processing_step = 1 : line_count - 1
		
		for current_line = processing_step + 1 : line_count
			
			aux = Matrix( current_line, processing_step ) / Matrix( processing_step, processing_step )
			Matrix( current_line, : ) = Matrix( current_line, : ) - aux * Matrix( processing_step, : )
			
		end
		
	end
	
end





