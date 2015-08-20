module Auxiliares( retirarCabecalho,
                               pegarLargura,
                               removerPadding,
                               fazerTriplas,
                               escreverNoArquivo,
                               rearranjarPixelList) where

import Data.List
import System.IO
import Data.Char

removerPadding :: [Char] -> Int -> [Char]
removerPadding [] _ = []
removerPadding imagem largura = let (atual, resto) = splitAt (largura*3) imagem
				    padding = mod (largura*3) 4  in
				       let proximasLinhas = drop padding resto  in
					       atual ++ (removerPadding proximasLinhas largura)

pegarLargura :: [Char] -> Int
pegarLargura [] = 0
pegarLargura cabecalho = let lista = map ord (take 4 (drop 18 cabecalho))  in  (lista!!0) + (lista!!1) * 256

fazerTriplas :: [Char] -> [ [Char] ]
fazerTriplas [] = [] 
fazerTriplas arquivo = (take 3 arquivo):(fazerTriplas (drop 3 arquivo) )

escreverNoArquivo arquivo [] = return ()
escreverNoArquivo arquivo lista = do
					hPrint arquivo (show (map ord (head lista)))
					escreverNoArquivo arquivo (tail lista)

rearranjarPixelList ::  Int -> [Char] -> [ [Char] ]
rearranjarPixelList largura porLinha = reverse (separarPixelList largura porLinha)

separarPixelList ::  Int -> [Char] -> [ [Char] ]
separarPixelList largura [] = []
separarPixelList largura pl = let s = splitAt largura pl in
				(fst s):(separarPixelList largura (snd s) )

retirarCabecalho :: [Char] -> ([Char],[Char])
retirarCabecalho [] = ([],[])
retirarCabecalho imagem = splitAt 54 imagem