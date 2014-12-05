import System.Environment (getArgs)
import System.IO 
import Data.List
import Utils

main = do
  args <- getArgs
  case args of
    [file] -> do
      -- sugestão de http://stackoverflow.com/questions/12903240/haskell-invalid-code-page-byte-sequence
      -- parece que não funciona sempre, depende do locale. funcionou aqui
      -- vale ler: https://www.haskell.org/haskellwiki/Dealing_with_binary_data
      handle <- openFile file ReadMode
      hSetEncoding handle utf8_bom
      conteudo <- hGetContents handle
      let nome_arq = take (length file - 4) file

      let (header, img) = pegaHeader conteudo
      let pixels = pegaRGB img
      let rgb = transpose pixels

      let larg = calcLargura header
      let rgb_linhas = map (pixelLista larg) rgb

      let nomes = map (nome_arq ++) ["_red.txt", "_green.txt", "_blue.txt"]
      arqRGB <- sequence [openFile file WriteMode | file <- nomes]
      sequence [escrever arq str | (arq, str) <- zip arqRGB rgb_linhas]
      sequence [hClose arq | arq <- arqRGB]
