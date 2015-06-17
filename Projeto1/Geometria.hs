module Geometria( volumeEsfera
                              , areaEsfera
                              , volumeCubo
                              , areaCubo
                              , areaCuboide
                              , volumeCuboide
                              , areaLateralCilindro
                              , areaTotalCilindro
                              , volumeCilindro
                              , areaLateralCone
                              , areaTotalCone
                              , volumeCone
                              , areaLateralTroncoCone
                              , areaTotalTroncoCone
                              , volumeTroncoCone
                              , areaEsferoideOblato
                              , volumeEsferoideOblato
                              , areaEsferoideProlato
                              , volumeEsferoideProlato
                             ) where

volumeEsfera :: Floating a => a -> a
volumeEsfera raio = (4.0 / 3.0) * pi * (raio ^ 3)

areaEsfera :: Floating a => a -> a
areaEsfera raio = 4 * pi * (raio ^ 2)

volumeCubo :: Num a => a -> a
volumeCubo lado = volumeCuboide lado lado lado

areaCubo :: Num a => a -> a
areaCubo lado = areaCuboide lado lado lado

volumeCuboide :: Num a => a -> a -> a -> a
volumeCuboide a b c = areaRetangulo a b * c

areaCuboide :: Num a => a -> a -> a -> a
areaCuboide a b c = areaRetangulo a b * 2 + areaRetangulo a c * 2 + areaRetangulo c b * 2

areaRetangulo :: Num a => a -> a -> a
areaRetangulo a b = a * b

areaLateralCilindro :: Floating a => a -> a -> a
areaLateralCilindro raio altura = 2 * pi * raio * altura

areaTotalCilindro :: Floating a => a -> a -> a
areaTotalCilindro raio altura = areaLateralCilindro raio altura + 2 * pi * (raio ^ 2)

volumeCilindro :: Floating a => a -> a -> a
volumeCilindro raio altura = pi * (raio ^ 2) * altura

areaLateralCone :: Floating a => a -> a -> a
areaLateralCone raio altura = pi * raio * sqrt (raio ^ 2 + altura ^ 2)

calculoSqrt :: Floating a => a -> a -> a
calculoSqrt raio altura = sqrt (raio ^ 2 + altura ^ 2) + raio

areaTotalCone :: Floating a => a -> a -> a
areaTotalCone raio altura = pi * raio * (calculoSqrt raio altura)

volumeCone :: Floating a => a -> a -> a
volumeCone raio altura = (1.0 / 3.0) * pi * (raio ^ 2) * altura

areaLateralTroncoCone :: Floating a => a -> a -> a
areaLateralTroncoCone raio altura = pi * raio * sqrt (raio ^ 2 + altura ^ 2)

areaTotalTroncoCone :: Floating a => a -> a -> a
areaTotalTroncoCone raio altura = pi * raio * (calculoSqrt raio altura)

volumeTroncoCone :: Floating a => a -> a -> a -> a
volumeTroncoCone raio1 raio2 altura = (1.0 / 3.0) * pi * altura * (raio1 ^ 2 + raio2 ^ 2 + raio1 * raio2)

ecentricidadeOblata :: Floating a => a -> a -> a
ecentricidadeOblata a b = (sqrt (a ^ 2 - b ^ 2)) / a

circunf :: Floating a => a -> a
circunf a = 2 * pi * a ^ 2

areaEsferoideOblato :: (Floating a, Ord a) => a -> a -> a
areaEsferoideOblato a b = if b < a
  then (circunf a) * (1 + (((1 - ((ecentricidadeOblata a b) ^ 2))  / ecentricidadeOblata a b) * atanh (ecentricidadeOblata a b)))
  else if b == a
  then areaEsfera a
  else 0

volumeEsferoideOblato :: Floating a => a -> a -> a
volumeEsferoideOblato a b = (4.0 / 3.0) * pi * a ^ 2 * b

ecentricidadeProlata :: Floating a => a -> a -> a
ecentricidadeProlata a b = (sqrt (b ^ 2 - a ^ 2)) / b

areaEsferoideProlato :: (Floating a, Ord a) => a -> a -> a
areaEsferoideProlato a b = if b > a
   then (circunf a) * (1 + ((b / a * (ecentricidadeProlata a b)) * (asin (ecentricidadeProlata a b))))
   else if b == a
   then areaEsfera a
   else 0

volumeEsferoideProlato :: Floating a => a -> a -> a
volumeEsferoideProlato a b = (4.0 / 3.0) * pi * a * b ^ 2
