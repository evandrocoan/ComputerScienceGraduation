import System.Environment (getArgs)
import System.IO
import Auxiliares
import qualified Data.List as List

main = do	
    fstr <- readFile "a.bmp"

    let (cabecalho, imagem) = retirarCabecalho fstr
    let largura = pegarLargura cabecalho
    let imagemSemPadding = removerPadding imagem largura
    let pixels = fazerTriplas imagemSemPadding
    let rgb = List.transpose pixels
    let rgbEmLinhas = map (rearranjarPixelList largura) rgb
    let nomeInitArq = take (length "a.bmp" - 4) "a.bmp"
    let nomesRGB = map (nomeInitArq++)["_red.txt","_green.txt","_blue.txt"]
    arquivosRGB <- sequence [openFile file WriteMode | file <- nomesRGB]

    sequence [escreverNoArquivo arq str | (arq, str) <- zip arquivosRGB rgbEmLinhas]
    sequence [hClose f | f <- arquivosRGB]

    putStr "separação terminada. Arquivos criados: \n"
    putStr $ concat $ map (++ "\n") nomesRGB
