-- Prática 01 de Haskell - Parte 2
-- Nome: Bento Borges Schirmer

import Basico
import Utilidades

itemize :: [String] -> [String]
itemize lista = map htmlItem lista

onlyVowels :: String -> String
onlyVowels = filter isVowel

onlyElderly :: [Int] -> [Int]
onlyElderly = filter isElderly

onlyLongWords :: [String] -> [String]
onlyLongWords = filter isLongWord

--onlyEven :: (Integral a) => [a] -> [a]
onlyEven :: [Int] -> [Int]
onlyEven = filter isEven

onlyBetween60and80 :: (Integral a) => [a] -> [a]
onlyBetween60and80 = filter between60and80
    where between60and80 age = age > 60 && age < 80

countSpaces :: String -> Int
countSpaces = length . filter (== ' ')

calcAreas :: (Floating a) => [a] -> [a]
calcAreas = map (\r -> r^2 * pi)

charFound :: Char -> String -> Bool
charFound _ [] = False
charFound letra (atual:resto)
    | letra == atual = True
    | otherwise      = charFound letra resto
