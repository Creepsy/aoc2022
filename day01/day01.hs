import Data.List (sort)
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split predicate toSplit = let (seg, toSplit') = span (/= predicate) toSplit in 
        if null toSplit' then
            [seg]
        else 
            seg : split predicate (drop 1 toSplit') 

solvePart1 :: [[Integer]] -> Integer
solvePart1 = maximum . map sum 

solvePart2 :: [[Integer]] -> Integer
solvePart2 = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = do
    elves <- map (map read) . split "" . lines <$> readFile "input.txt" :: IO [[Integer]]

    putStrLn $ "Part 1: " ++ show (solvePart1 elves)
    putStrLn $ "Part 2: " ++ show (solvePart2 elves)