import Data.List
import System.IO
import Utils
import PDL


estados = ['a', 'b', 'c']
transicao :: Char -> [(Char, Char)]
transicao aresta
    | aresta == 'α' = [('a', 'a'), ('a', 'b')]
    | aresta == 'β' = [('c', 'b'), ('b', 'c')]
    | otherwise = error "transição inválida"

programa = ["α;", "α U β", "β*"]







