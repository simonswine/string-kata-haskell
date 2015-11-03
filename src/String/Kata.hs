module String.Kata (calc, split, getUpTo) where


calc :: [Char] -> Integer
calc a = summe(convertToInt(split a ','))

split :: [Char] -> Char -> [[Char]]
split string needle
    | first == "" && recurse == "" = []
    | recurse == "" = [first]
    | otherwise = [first] ++ split recurse needle
    where (first, recurse) = getUpTo("", string) needle

getUpTo :: ([Char],[Char]) -> Char -> ([Char], [Char])
getUpTo ("", "") _ = ("", "")
getUpTo (left, x:right) needle
    | x == needle = (left, right)
    | right == "" = (left ++ [x], right)
    | otherwise = getUpTo (left ++ [x], right) needle

convertToInt :: [[Char]] -> [Integer]
convertToInt ([]) = []
convertToInt (x:list) = [read x] ++ convertToInt list

summe :: [Integer] -> Integer
summe = foldr (+) 0
