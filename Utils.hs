module Utils (
  pegaHeader,
  calcLargura,
  pixelLista,
  pegaRGB,
  escrever
) where

import Data.Char
import Data.List
import System.IO

-- separa cabeçalho da imagem (primeiros 54b)
pegaHeader :: [Char] -> ([Char], [Char])
pegaHeader [] = ([], [])
pegaHeader img = splitAt 54 img

-- largura está do byte 18 ao 22
calcLargura :: [Char] -> Int
calcLargura [] = 0
-- pega os bytes da largura e multiplica pela potencia da posição (hex)
calcLargura header =
  let lista = map ord (take 4 (drop 18 header)) in
    (lista !! 0) + (lista !! 1) * 256 -- 256 = 16 ** 2

-- separar pixels
separarPixels :: Int -> [Char] -> [[Char]]
separarPixels larg [] = []
separarPixels larg l =
  let s = splitAt larg l in
    (fst s) : (separarPixels larg (snd s))

-- transforma os pixels em lista
pixelLista :: Int -> [Char] -> [[Char]]
pixelLista larg linha = reverse (separarPixels larg linha)

-- pega de 3 em 3 bytes (r, g, b)
pegaRGB :: [Char] -> [[Char]]
pegaRGB [] = []
-- pega os 3 bytes e passa adiante recursivamente
pegaRGB arq = (take 3 arq) : (pegaRGB (drop 3 arq))

-- escreve no arquivo recursivamente
escrever arq [] = return ()
escrever arq lista = do
  hPrint arq (show (map ord (head lista)))
  escrever arq (tail lista)
