module Listas ( primeiros,
                         calculaSomaPA,
                         calculaProdPA,
                         mapeiaListaPA
                        ) where

primeiros 0 _ = []
primeiros n [] = []
primeiros n (a:x) = a : primeiros (n - 1) x

fact = foldl (*) 1 . enumFromTo 1

-- a0 = 0

calculaSomaPA n razao = sum (map (\x -> razao + ((x - 1) * razao)) (take n [1..]))

-- a0 = 0

mapeiaListaPA n razao = map (\x -> razao + ((x - 1) * razao)) (take n [1..])

-- a0 = 1

calculaProdPA n razao = product (map (\x -> razao + ((x - 1) * razao)) (take n [1..]))
