import Data.Function (on)
import Data.List (sortBy)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (s1:string1) [] = score s1 '-' * length (s1:string1)
similarityScore [] (s2:string2) = score '-' s2 * length (s2:string2)
similarityScore (s1:string1) (s2:string2) = maximum [similarityScore string1 string2 + score s1 s2, similarityScore string1 (s2:string2) + score s1 '-', similarityScore (s1:string1) string2 + score '-' s2]


score x y
 | x == '-' || y == '-' = scoreSpace
 | x == y = scoreMatch
 | otherwise = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

stringScore :: (String, String) -> Int
stringScore ([], []) = 0
stringScore (x:xs, y:ys) = score x y + stringScore (xs, ys)


outputOptAlignments string1 string2 = do
  let opts = optAlignmentsSlow string1 string2
  putStrLn $ "There are " ++ (show . length $ opts) ++ " optimal alignments."
  putStrLn $ concat (map outputSingle opts)

outputSingle (string1, string2) = "\n"++string1 ++ "\n"++string2 ++ "\n"


optAlignmentsSlow :: String -> String -> [AlignmentType]
optAlignmentsSlow xs ys = maximaBy stringScore $ findAlignments xs ys

findAlignments [] [] = [("","")]
findAlignments [] ys = [(replicate (length ys) '-', ys)]
findAlignments xs [] = [(xs, replicate (length xs) '-')]
findAlignments (x:xs) (y:ys) = concat [
      attachHeads x y $ findAlignments xs ys,
      attachHeads x '-' $ findAlignments xs (y:ys),
      attachHeads '-' y $ findAlignments (x:xs) ys]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy func xs = foldr (maxima func) [] xs

maxima func x [] = [x]
maxima func x (a:acc)
 | func x >  func a = [x]
 | func x == func a = (x:a:acc)
 | otherwise = (a:acc)
