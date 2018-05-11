import Data.Function (on)
import Data.List (sortBy)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)


similarityScoreOld :: String -> String -> Int
similarityScoreOld [] [] = 0
similarityScoreOld (s1:string1) [] = score s1 '-' * length (s1:string1)
similarityScoreOld [] (s2:string2) = score '-' s2 * length (s2:string2)
similarityScoreOld (s1:string1) (s2:string2) = maximum [similarityScoreOld string1 string2 + score s1 s2, similarityScoreOld string1 (s2:string2) + score s1 '-', similarityScoreOld (s1:string1) string2 + score '-' s2]

similarityScore :: String -> String -> Int
similarityScore string1 string2 = simLen (length string1) (length string2)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry s1 0 = scoreSpace * s1
    simEntry 0 s2 = s2 * scoreSpace
    simEntry i j = maximum [simLen (i-1) (j-1)+ score (string1 !! (l1 - i)) (string2 !! (l2 - j)),
            simLen (i-1) j + scoreSpace,
            simLen i (j-1) + scoreSpace]
        where
          l1 = length string1
          l2 = length string2

--optAlignments :: String -> String -> [AlignmentType]
--optAlignments string1 string2 = snd $ simLen (length string1) (length string2)
--  where
--    simLen i j = simTable!!i!!j
--    simTable = [[ alignmentEntry i j | j<-[0..]] | i<-[0..] ]
--
--    alignmentEntry 0 0 = (0,[("","")])
--    alignmentEntry 0 ys = (ys * scoreSpace, [(replicate ys '-', takeLast ((length string2) - ys) string2)])
--    alignmentEntry xs 0 = (xs * scoreSpace, [(takeLast ((length string1) - xs) string1, replicate xs '-')])
--    alignmentEntry xs ys = 1 !! res
--       where
--         x = string1 !! ((length string1) - xs)
--         y = string2 !! ((length string2) - ys)
--         entry1 = attachOnPair (score x y) x y simLen (xs-1) (ys-1)
--         entry2 = attachOnPair scoreSpace x '-' simLen (xs-1) ys
--         entry3 = attachOnPair scoreSpace '-' y simLen xs (ys-1)
--         res = maximaBy fst [entry1, entry2, entry3]

-- [(1,alignmenttype), ]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 li = [((xs ++ [h1]), (ys ++ [h2])) | (xs,ys) <- li]

fastOptAlignments :: String -> String -> [AlignmentType]
fastOptAlignments xs ys = snd $ optAlign (length xs) (length ys)
    where
    optAlign i j = optTable!!i!!j
    optTable :: [[(Int, [AlignmentType])]]
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..]]

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = (scoreSpace + fst (optEntry (i-1) 0), attachTails (xs!!(i-1)) '-' $ snd (optEntry (i-1) 0))
    optEntry 0 j = (scoreSpace + fst (optEntry 0 (j-1)), attachTails '-' (ys!!(j-1)) $ snd (optEntry 0 (j-1)))
    optEntry i j = (fst (best!!0), concat $ map snd best)
        where
        best = maximaBy fst [diag, right, down]
            where
            diag, right, down :: (Int, [AlignmentType])
            diag
                | x == y    = addScoreAndTails scoreMatch x y (optAlign (i-1) (j-1))
                | otherwise = addScoreAndTails scoreMismatch x y (optAlign (i-1) (j-1))
            right = addScoreAndTails scoreSpace '-' y (optAlign i (j-1))
            down = addScoreAndTails scoreSpace x '-' (optAlign (i-1) j)
            x = xs!!(i-1)
            y = ys!!(j-1)

            addScoreAndTails :: Int -> Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
            addScoreAndTails sc a b (score, as) = (score + sc, attachTails a b as)

--attachHeads x y $ snd $ alignmentEntry (xs-1) (ys-1),
--attachHeads x '-' $ snd $ alignmentEntry (xs-1) ys,
--attachHeads '-' y $ snd $ alignmentEntry xs (ys-1)]

attachOnPair s x y (score, aTypes) = (s + score, attachHeads x y $ aTypes)

takeLast n = reverse . take n . reverse

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
