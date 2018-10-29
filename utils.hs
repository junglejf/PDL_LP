module Utils  where
import Data.List
import System.IO

removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas = foldl (\seen x -> if x `elem` seen
    then seen
    else seen ++ [x]) []

concatenarDuasListasSemRepeticao :: [Int] -> [Int] -> [Int]
concatenarDuasListasSemRepeticao lista1 lista2 = [x | x <- removeDuplicatas (lista1 ++ lista2) ]


listasIguais :: (Eq a) => [a] -> [a] -> Bool
listasIguais x y = null (x \\ y) && null (y \\ x)


