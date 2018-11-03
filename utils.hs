module Utils  where
import Data.List
import System.IO

removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas = foldl (\seen x -> if x `elem` seen
    then seen
    else seen ++ [x]) []

concatenarDuasListasSemRepeticao ::  (Eq a) => [a] -> [a] -> [a]
concatenarDuasListasSemRepeticao lista1 lista2 =  removeDuplicatas (lista1 ++ lista2) 

listasIguais :: (Eq a) => [a] -> [a] -> Bool
listasIguais x y = null (x \\ y) && null (y \\ x)


deletes :: Eq a => a -> [a] -> [a]
deletes deleted xs = [ x | x <- xs, x /= deleted]


arraySize :: Eq a => [a] -> Int
arraySize (x:xs)
    | xs == [] = 1
    | otherwise = 1 + arraySize xs
