import Utilities;

myMap2 = map2 ((+2),(+3)) (3,7)


func a = a * 2

testme = fix (/2) 10

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (x:t) s
 | wildcard == x = s  ++ (substitute wildcard t s)
 | otherwise = x: (substitute wildcard t s)
