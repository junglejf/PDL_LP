import Data.List
import System.IO
import Utils
import PDL

estados = [1, 2, 3, 4, 5]

-- transicao :: Eq a => a -> [(a, a)]
-- transicao programa 
--     | programa == "alpha" = [(1, 1), (1, 2), (2, 3), (3, 4), (4, 4)]
--     | programa ==  "beta" = [(1, 2), (2, 1), (3, 3), (5, 5), (5, 4)]
--     | otherwise = error "trasição não definida"

transicaoAlpha  = [(1, 1), (1, 2), (2, 3), (3, 4), (4, 4)]
transicaoBeta = [(1, 2), (2, 1), (3, 3), (5, 5), (5, 4)]
-- transicoes vao ser uma funcao

-- verificacaoVariavel :: Eq -> [Eq]
-- verificacaoVariavel variavel 
--     | variavel == "p" = [1, 2, 3, 4]
--     | variavel == "q" = [1, 5]
--     | otherwise = error "variável não definida"

validaP = [1, 2, 3, 4]
validaQ = [1, 5]


estadoAtual = [1]


t1 = executaTransicao estadoAtual transicaoAlpha
t2 = executaTransicao [3] transicaoAlpha

t31 = executaTransicao [1] transicaoAlpha
t32 = executaNTransicoes [1] transicaoAlpha

t41  = executaTransicao [1] transicaoBeta
t42  = executaNTransicoes [1] transicaoBeta

t61 = executaTransicao [4] transicaoAlpha
t62 = executaNTransicoes [4] transicaoAlpha

t51 = executaTransicao [4] transicaoBeta
t52 = executaNTransicoes [4] transicaoBeta


w1 = checaVariavelValida validaP [1] False
w2 = checaVariavelValida validaQ [1] False

w3 = checaVariavelValida validaP [1, 5] False
w4 = checaVariavelValida validaQ [5] False

w5 = checaVariavelValida validaP [1, 2, 3, 4] False









