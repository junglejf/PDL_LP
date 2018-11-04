import Data.List
import System.IO
import Utils
import PDL
import Debug.Trace


estados = ['a', 'b', 'c']
estadoAtual = head estados
transicao :: Char -> [(Char, Char)]
transicao aresta
    | aresta == 'α' = [('a', 'a'), ('a', 'b')]
    | aresta == 'β' = [('a', 'b')]
    | otherwise = error "transição inválida"

programa = ["α*","β*"]


deletes :: Eq a => a -> [a] -> [a]
deletes deleted xs = [ x | x <- xs, x /= deleted]

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
    | containsEdge x 'U' = ((checarExecutaTransicao2 (head x)  estadoAtual) || (executaLoop [tail(tail(x ++ ";"))] estadoAtual)) && executaLoop xs (checarExecutaTransicao (head x)  estadoAtual)
    | containsEdge x ';' = if (checarExecutaTransicao2 (head x)  estadoAtual) == False then error "erro nos estados" else executaLoop xs (checarExecutaTransicao (head x)  estadoAtual) 
    | containsEdge x '*' = any (\elemento -> executaLoop xs elemento)(executaNTransicoes2 [estadoAtual] (transicao(head x)) 1)
    | otherwise = error "Programa inválido"

    -- splitOn "," "my,comma,separated,list"
    -- ["my","comma","separated","list"]