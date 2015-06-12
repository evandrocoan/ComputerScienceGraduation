module Geometria.Cuboide( volume
                                             , area
                                            ) where

volume :: Float -> Float -> Float -> Float
volume a b c = areaRetangulo a b * c

area :: Float -> Float -> Float -> Float
area a b c = areaRetangulo a b * 2 + areaRetangulo a c * 2 + areaRetangulo c b * 2

areaRetangulo :: Float -> Float -> Float
areaRetangulo a b = a * b
