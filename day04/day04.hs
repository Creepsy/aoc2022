import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

type Range = (Int, Int)

main :: IO ()
main = do
    pairs <- map parsePair . lines <$> readFile "input.txt"
    
    putStrLn $ "Part 1: " ++ show (solvePart1 pairs)
    putStrLn $ "Part 2: " ++ show (solvePart2 pairs)

solvePart1 :: [(Range, Range)] -> Int
solvePart1 = foldl (\acc contained -> if contained then acc + 1 else acc) 0 . map (uncurry oneFullyContainedInOther) 

solvePart2 :: [(Range, Range)] -> Int
solvePart2 = length . filter isJust . map (uncurry intersect)

oneFullyContainedInOther :: Range -> Range -> Bool
oneFullyContainedInOther first second = isJust intersection && (fromJust intersection == first || fromJust intersection == second)
    where intersection = first `intersect` second

intersect :: Range -> Range -> Maybe Range
intersect (fStart, fEnd) (sStart, sEnd)
    | fStart > sEnd || sStart > fEnd = Nothing
    | otherwise = Just (max fStart sStart, min fEnd sEnd)


parsePair :: String -> (Range, Range)
parsePair pairStr = let [fElf, sElf] = map parseRange . splitOn "," $ pairStr in (fElf, sElf)

parseRange :: String -> Range
parseRange rangeStr = let [start, end] = map read . splitOn "-" $ rangeStr in (start, end)