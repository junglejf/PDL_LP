import Data.List
import System.IO
import Utils
import PDL

estados = [1, 2, 3, 4, 5]
transicaoAlpha  = [(1, 1), (1, 2), (2, 3), (3, 4), (4, 4)]
transicaoBeta = [(1, 2), (2, 1), (3, 3), (5, 5), (5, 4)]
-- transicoes vao ser uma funcao

estadoAtual = [1]

t1 = executaTransicao estadoAtual transicaoAlpha
t2 = executaTransicao [3] transicaoAlpha
t3 = executaNTransicoes [1] transicaoAlpha
t4  = executaNTransicoes [1] transicaoBeta











