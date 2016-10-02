module Main (main) where

import Control.Monad
import Hiberbolicas

prompt x = do
                putStrLn x
                number <- getLine
                return number


main = do
    putStr "\ESC[2J"
    calculadora


calculadora = do
    putStrLn ( "" )  
    putStrLn "Bem vindo a calculadora Hiberbolica!!!! \n\
                        \Digite: \n\
                        \a para senoHiperbolico, \n\
                        \b para cossenoHiperbolico, \n\
                        \c para tangenteHiperbolicoco, \n\
                        \d para cotangenteHiperbolico, \n\
                        \\n\
                        \t para limpar a tela, \n\
                        \s para sair, \n\
                        \Insira uma opcao: "
    line <- getLine
    let boo = ( line /= "s")
    let boo2 = if boo then "True!!!!!" else "False!!!!! "
    putStrLn ( "" )  
    if boo then do
        case line of
            "a" -> do
                line <- prompt "Insira o angulo senoHiperbolico: "
                let lineFloat  = tangenteHiperbolico (read line :: Float)
                print ( "Resposta: " ++ show lineFloat )
            "b" ->  do
                line <- prompt "Insira o angulo cossenoHiperbolico: "
                let lineFloat  = tangenteHiperbolico (read line :: Float)
                print ( "Resposta: " ++  show lineFloat )
            "c" -> do
                line <- prompt "Insira o angulo tangenteHiperbolico: "
                let lineFloat  = tangenteHiperbolico (read line :: Float)
                print ( "Resposta: " ++ show lineFloat )
            "d" -> do
                line <- prompt "Insira o angulo cotangenteHiperbolico: "
                let lineFloat  = cotangenteHiperbolico (read line :: Float)
                print ( "Resposta: " ++ show lineFloat )
            "t" -> putStrLn $ "\ESC[2J"
            _ -> do
                putStrLn ( show boo2 ++ " Valor invalido inserido!!!!!!!!!!!!!!" )
        putStrLn ( "FIM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" )
        calculadora
    else do
        putStrLn ( "VOLTE SEMPRE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")  