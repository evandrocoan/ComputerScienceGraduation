{-| 
	Roteiro 1
	Dado o c�digo abaixo, use a indenta��o correta para substituir a pontua��o:
	f x = case x of
	{ 0 -> 1; 1 -> 5; 2 -> 2; _ -> 1 }
-}

f x = case x of
	0 -> 1
	1 -> 5
	2 -> 2
	_ -> 1

{-| 
	Fa�a o inverso com o c�digo abaixo, re-escrevendo-o na forma pontuada:
	quicksort :: (Ord a) => [a] -> [a]
	quicksort [] = []
	quicksort (x:xs) =
		quicksort lt ++ [x] ++ quicksort ge
		where
			lt = [y | y <- xs, y < x]
			ge = [y | y <- xs, y >= x]
-}
quicksort :: (Ord a) => [a] -> [a];quicksort [] = [];quicksort (x:xs)  = quicksort lt ++ [x] ++ quicksort ge	where	{ lt = [y | y <- xs, y < x]; ge = [y | y <- xs, y >= x] } 

{-|
	Roteiro 2
	Usando o operador �..� encontre:
	1. Uma lista de 1 a 1000;
	2. Uma PA de 1 a 99 de raz�o 3;
	3. Uma PG de 50 termos de raz�o 2;
	4. O n-�simo termo de uma s�rie infinita de fatoriais.
-}

questao1 = [1..1000]
questao2 = [1,4..99]
questao3 = map (2^) [1..50]
questao4 = product [1..50]