import Data.List
import System.IO
import Utils
import PDL
import Debug.Trace



foo a b c = trace ("input a: " ++ show a) (a + b + c)
main = print (foo 1 2 3)


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


checarExecutaTransicao2 :: Char -> [Char] -> Bool
checarExecutaTransicao2 paramTransicao estadosAtuais
    | elem '∅' novosEstados = False
    | otherwise = True
    where
        novosEstados = executaTransicao estadosAtuais (transicao paramTransicao)



executaLoop :: [String] -> [Char] -> Bool
executaLoop [] estadoAtual = True
executaLoop (x:xs) estadoAtual
    | containsEdge x ';' =  (checarExecutaTransicao2 (head x)  estadoAtual) && executaLoop xs (checarExecutaTransicao (head x)  estadoAtual) 
    | containsEdge x 'U' =   False
    | containsEdge x '*' = False
    | otherwise = error "Programa inválido"




    -- splitOn "," "my,comma,separated,list"
    -- ["my","comma","separated","list"]



