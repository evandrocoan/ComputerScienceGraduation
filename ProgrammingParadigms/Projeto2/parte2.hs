module Infinitas ( quaseUm, 
                             quaseDois, 
                             quaseE
                           ) where

fat 0 = 1
fat n = n * fat(n-1)

quaseUm n = sum (map (\x->(1/(x^2+x))) [1..n])
quaseDois n = sum (map (\x->(1/2^x)) [0..n])
quaseE n = sum (map (\x -> 1/fat x) [0..n])
