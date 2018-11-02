import Data.List
import System.IO
import Data.List.Split
import Utils
import PDL

estados = ['a', 'b', 'c']
estadoAtual = head estados
transicao :: Char -> [(Char, Char)]
transicao aresta
    | aresta == 'α' = [('a', 'a'), ('a', 'b')]
    | aresta == 'β' = [('c', 'b'), ('b', 'c')]
    | otherwise = error "transição inválida"

programa = ["α;", "α U β", "β*"]


containsEdge ::  String -> Char -> Bool
containsEdge programa digito = digito `elem` programa



checarExecutaTransicao :: Char -> [Char] -> [Char]
checarExecutaTransicao paramTransicao estadosAtuais
    | elem '∅' novosEstados = error "Transição com falhou para os estados atuais " ++ estadosAtuais 
    | otherwise = novosEstados
    where
        novosEstados = executaTransicao estadosAtuais (transicao paramTransicao)


executaLoop :: [String] -> [Char] -> Bool
executaLoop [] estadoAtual = True
executaLoop (x:xs) estadoAtual
    | containsEdge x ';' =  executaLoop xs (checarExecutaTransicao (head x)  estadoAtual) 
	| containsEdge x 'U' =   False
    | containsEdge x '*' = False
    | otherwise = error "Programa inválido"
    

  


    -- splitOn "," "my,comma,separated,list"
    -- ["my","comma","separated","list"]



