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


executaNTransicoes2 :: [[Char]] -> [(Char, Char)] -> Int -> [[Char]]
executaNTransicoes2 estadosAtuais transicoes i
    | transicaoExecutada == ['∅'] = estadosAtuais
    | i > arraySize transicoes = estadosAtuais
    | otherwise = removeDuplicatas(executaNTransicoes2 (estadosAtuais ++ [(deletes '∅' transicaoExecutada)]) transicoes (i+1))
    where 
        transicaoExecutada =  removeDuplicatas(executaTransicao (last estadosAtuais) transicoes)