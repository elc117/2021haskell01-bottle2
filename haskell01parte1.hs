-- Prática 01 de Haskell - Parte 1
-- Nome: Bento Borges Schirmer

module Utilidades
( htmlItem
, isVowel
, isElderly
) where

sumSquares :: Int -> Int -> Int
sumSquares x y = x^2 + y^2

circleArea :: Float -> Float
circleArea r = r^2 * pi

age :: Int -> Int -> Int
age ano_nascimento ano_atual = ano_atual - ano_nascimento

isElderly :: Int -> Bool
isElderly idade = idade > 65

htmlItem :: String -> String
htmlItem item = "<li>" ++ item ++ "</li>"

startsWithA :: String -> Bool
startsWithA string = head string == 'A'

isVerb :: String -> Bool
isVerb palavra = last palavra == 'r'

isVowel :: Char -> Bool
isVowel letra = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads lista_1 lista_2 = head lista_1 == head lista_2

isVowelBetter :: Char -> Bool
isVowelBetter letra = elem letra "aeiou"

isVowelDifferent :: Char -> Bool
isVowelDifferent 'a' = True
isVowelDifferent 'e' = True
isVowelDifferent 'i' = True
isVowelDifferent 'o' = True
isVowelDifferent 'u' = True
isVowelDifferent x = False

isVowelDifferenter :: Char -> Bool
isVowelDifferenter = (`elem` "aeiou")
