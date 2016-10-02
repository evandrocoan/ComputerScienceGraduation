module Geometria.Esfera( volume
                                          , area 
                                        ) where

volume :: Float -> Float
volume raio = (4.0 / 3.0) * pi * (raio ^ 3)

area :: Float -> Float
area raio = 4 * pi *(raio ^ 2)
