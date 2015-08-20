import Data.List

doubleMe x = x + x 
lostNumbers = [4,8,15,16,23,42]  
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  
questao3 = [ x | x <- [1..4], y <- [x..5], (x+y) `mod` 2 == 0 ]
length2 xs = sum [1 | _ <- xs]   
minhaLista = [5..10]
quad = \x->x*x
expr = \x->x^2+2*x+3
raiz = \x->(sqrt x)
teste1 = map (\x->x*x) [1..10]
f x = x*x
teste2 = map f [1..10]
fac n = if n==1 then 1 else (n*fac(n-1))
teste3  = map (\x->(fac x)) [1..10]
----------------------(a -> a -> Bool) -> a -> [a] -> [a]
--questao2 lista = [ if  then  else | x <- lista, x `mod` 4 == 0 ]  
questao2 = [ x | x <- [4..19], x `mod` 4 /= 0 ]
g x y = \x y-> ( y `mod` x ) == 0
--questao22 = deleteFirstsBy  [3..4] [5..10] 

questao222 = filter (\n -> n `mod` 4 == 0) [4..19]









