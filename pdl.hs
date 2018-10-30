module PDL where
import Utils

executaTransicao :: [Int] -> [(Int, Int)] -> [Int]
executaTransicao estadosAtuais transicoes = [snd x | x <- transicoes, elem (fst x) estadosAtuais]

executaNTransicoes :: [Int] -> [(Int, Int)] -> [Int]
executaNTransicoes estadosAtuais transicoes 
    | transicaoExecutada == [] = []
    | listasIguais listasConcatenadas  estadosAtuais = estadosAtuais
    | otherwise = executaNTransicoes listasConcatenadas transicoes
    where 
        transicaoExecutada = executaTransicao estadosAtuais transicoes
        listasConcatenadas = concatenarDuasListasSemRepeticao transicaoExecutada estadosAtuais


--TODO:checar o caso do aceita vazio
checaVariavelValida :: [Int] -> [Int] -> Bool -> Bool
checaVariavelValida verificacaoVariavel estadosAtuais aceitaVazio
    | estadosAtuais == [] = aceitaVazio
    | listasIguais listaElementosDentro estadosAtuais = True 
    | otherwise = False  
    where 
        listaElementosDentro = [x | x <- estadosAtuais, elem x verificacaoVariavel]



