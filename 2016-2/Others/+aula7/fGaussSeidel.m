function Solution = fGaussSeidel( Matrix, degree )
    
    for i = 1 : degree
        
        inicialSolution( i ) = 0;
        
    end
    
    inicialSolution
    currentSolution = inicialSolution;
    
    currentStep  = 0
    maximumSteps = 1000
    
    currentError = 1
    desiredError = 1e-10
    
    while( currentStep < maximumSteps && currentError < desiredError )
        
        currentStep = currentStep + 1
        
        
        
        
        currentError    = max( inicialSolution .- currentSolution )
        inicialSolution = currentSolution
        
    end
    
end
