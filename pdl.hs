module PDL where
import Utils

executaTransicao :: [Char] -> [(Char, Char)] -> [Char]
executaTransicao estadosAtuais transicoes = [snd x | x <- transicoes, elem (fst x) estadosAtuais] ++ ['∅' | x <- estadosAtuais, not(elem x  (map fst transicoes))]

executaNTransicoes :: [Char] -> [(Char, Char)] -> [Char]
executaNTransicoes estadosAtuais transicoes 
    | transicaoExecutada == [] = []
    | listasIguais listasConcatenadas  estadosAtuais = estadosAtuais
    | otherwise = executaNTransicoes listasConcatenadas transicoes
    where 
        transicaoExecutada = executaTransicao estadosAtuais transicoes
        listasConcatenadas = concatenarDuasListasSemRepeticao transicaoExecutada estadosAtuais

