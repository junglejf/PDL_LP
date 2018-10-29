module PDL where
import Utils

executaTransicao :: [Int] -> [(Int, Int)] -> [Int]
executaTransicao estadosAtuais transicoes = [snd x | x <- transicoes, elem (fst x) estadosAtuais]

executaNTransicoes :: [Int] -> [(Int, Int)] -> [Int]
executaNTransicoes estadosAtuais transicoes = 
    if listasIguais (concatenarDuasListasSemRepeticao (executaTransicao estadosAtuais transicoes) estadosAtuais)  estadosAtuais
        then estadosAtuais
        else executaNTransicoes (concatenarDuasListasSemRepeticao (executaTransicao estadosAtuais transicoes) estadosAtuais) transicoes