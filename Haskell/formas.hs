module Formas ( Forma ( Retangulo , Elipse , TriangRet ,
                                            Poligono
                                            ),
                                Raio , Lado , Vertice ,
                                quadrado , circulo , distEntre , area
                            ) where

data Forma = Retangulo Lado Lado
                    | Elipse Raio Raio
                    | TriangRet Lado Lado
                    | Poligono [ Vertice ]
        deriving Show

type Raio = Float
type Lado = Float
type Vertice = (Float , Float)

quadrado s = Retangulo s s
circulo r = Elipse r r

triArea :: Vertice -> Vertice -> Vertice -> Float
triArea v1 v2 v3 = let 
    a = distEntre v1 v2
    b = distEntre v2 v3
    c = distEntre v3 v1
    s = 0.5*(a+b+c)
    in sqrt (s*(s-a)*(s-b )*(s-c)) 

distEntre :: Vertice -> Vertice -> Float
distEntre (x1 ,y1) (x2 ,y2) = sqrt ((x1 -x2 )^2 + (y1 -y2 )^2)

area :: Forma -> Float
area ( Retangulo s1 s2) = s1*s2
area ( TriangRet s1 s2) = s1*s2 /2
area ( Elipse r1 r2) = pi*r1*r2