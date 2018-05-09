similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (s1:string1) [] = score s1 '-' * length string1
similarityScore [] (s2:string2) = score '-' s2 * length string2
similarityScore (s1:string1) (s2:string2) = maximum [similarityScore string1 string2 + score s1 s2, similarityScore string1 (s2:string2) + score s1 '-', similarityScore (s1:string1) string2 + score '-' s2]


score x y
 | x == '-' || y == '-' = -2
 | x == y = 1
 | otherwise = -1
