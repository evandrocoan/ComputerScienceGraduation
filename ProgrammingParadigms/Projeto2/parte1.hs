module Finitas ( somaInteiros, 
                           somaPares, 
                           somaImpares,
                           somaQuadrados, 
                           somaQuadradosPares, 
                           somaQuadradosImpares 
                        ) where

somaInteiros n = n*(n+1)/2
somaPares = (*) 2 . somaInteiros
somaImpares n = somaInteiros (2*n) - somaPares n

somaQuadrados n = n * (n+1) * (2*n+1) / 6
somaQuadradosPares = (*) 4 . somaQuadrados
somaQuadradosImpares n = somaQuadrados (2*n) - somaQuadradosPares n
