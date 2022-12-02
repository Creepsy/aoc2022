import Data.Char (ord)

type Round = (Char, Char)

parseRound :: String -> Round
parseRound roundStr = let [f, _, s] = roundStr in (f, s)
 
playRoundPart1 :: Round -> Integer
playRoundPart1 round@(enemy, you) = typeScore + winScore
    where typeScore = fromIntegral $ 1 + ord you - ord 'X'
          winScore = case round of
                ('A', 'X') -> 3
                ('A', 'Y') -> 6
                ('A', 'Z') -> 0
                ('B', 'X') -> 0
                ('B', 'Y') -> 3
                ('B', 'Z') -> 6
                ('C', 'X') -> 6
                ('C', 'Y') -> 0
                ('C', 'Z') -> 3

playRoundPart2 :: Round -> Integer
playRoundPart2 round@(enemy, result) = typeScore + winScore
    where typeScore = case round of
                ('A', 'X') -> 3
                ('A', 'Y') -> 1
                ('A', 'Z') -> 2
                ('B', 'X') -> 1
                ('B', 'Y') -> 2
                ('B', 'Z') -> 3
                ('C', 'X') -> 2
                ('C', 'Y') -> 3
                ('C', 'Z') -> 1
          winScore = case result of
                'X' -> 0
                'Y' -> 3
                'Z' -> 6
          

solvePart1 :: [Round] -> Integer
solvePart1 = sum . map playRoundPart1

solvePart2 :: [Round] -> Integer
solvePart2 = sum . map playRoundPart2

main :: IO ()
main = do
    rounds <- map parseRound . lines <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show (solvePart1 rounds)
    putStrLn $ "Part 2: " ++ show (solvePart2 rounds)