{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Text.Parsec ((<?>), (<|>), many, endBy, anyChar, endOfLine, spaces, string, sepBy1, char, many1, digit)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Vector.Mutable as MV
import Data.Either (isLeft, fromRight, fromLeft)
import Data.Vector.Mutable (IOVector)
import Control.Monad (zipWithM_, replicateM)
import Data.List (sort, transpose)

data Monkey = Monkey {
    monkeyID :: Int,
    initialItems :: [Int],
    operation :: Int -> Int,
    divisibleTest :: Int,
    onTrue :: Int,
    onFalse :: Int
} 

main :: IO ()
main = do
    monkeys <- parseFromFile parseMonkeys "input.txt"

    if isLeft monkeys then
        print $ fromLeft undefined monkeys
    else do
        let monkeys' = fromRight [] monkeys

        part1Solution <- solvePart1 monkeys'
        part2Solution <- solvePart2 monkeys'

        putStrLn $ "Part 1: " ++ show part1Solution
        putStrLn $ "Part 2: " ++ show part2Solution


solvePart1 :: [Monkey] -> IO Int
solvePart1 monkeys = do
    items <- MV.generate (length monkeys) (\index -> initialItems (monkeys !! index))

    business <- reverse . sort . map sum . transpose <$> replicateM 20 (playRound calculateNewWorryLevelPart1 items monkeys)

    return . product . take 2 $ business

solvePart2 :: [Monkey] -> IO Int
solvePart2 monkeys = do
    items <- MV.generate (length monkeys) (\index -> initialItems (monkeys !! index))

    let reductionFactor = product . map divisibleTest $ monkeys

    business <- reverse . sort . map sum . transpose <$> replicateM 10000 (playRound (calculateNewWorryLevelPart2 reductionFactor) items monkeys)

    return . product . take 2 $ business

playRound :: (Monkey -> Int -> Int) -> IOVector [Int] -> [Monkey] -> IO [Int]
playRound worryLevelFunc items = mapM (handleMonkey worryLevelFunc items)

handleMonkey :: (Monkey -> Int -> Int) -> IOVector [Int] -> Monkey -> IO Int
handleMonkey worryLevelFunc items monkey = do
    inventory <- MV.exchange items (monkeyID monkey) []

    let inventory' = map (worryLevelFunc monkey) inventory
        targetMonkeys = map (evaluateTargetMonkey monkey) inventory'

    zipWithM_ (addItem items) targetMonkeys inventory'

    return $ length inventory
    
addItem :: IOVector [a] -> Int -> a -> IO ()
addItem toModify index value = MV.modify toModify (value:) index

evaluateTargetMonkey :: Monkey -> Int -> Int
evaluateTargetMonkey (Monkey _ _ _ divTest onTrue onFalse) worryLevel = if worryLevel `mod` divTest == 0 then onTrue else onFalse

calculateNewWorryLevelPart1 :: Monkey -> Int -> Int
calculateNewWorryLevelPart1 (Monkey _ _ operation _ _ _) worryLevel = operation worryLevel `div` 3

calculateNewWorryLevelPart2 :: Int -> Monkey -> Int -> Int
calculateNewWorryLevelPart2 reductionFactor (Monkey _ _ operation _ _ _) worryLevel = operation worryLevel `mod` reductionFactor

-- parsing

parseMonkeys :: Parser [Monkey]
parseMonkeys = many (spaces >> parseMonkey)

parseMonkey :: Parser Monkey
parseMonkey = do
    string "Monkey" >> spaces 
    monkeyID <- parseInt
    string ":"

    spaces >> string "Starting items:" >> spaces
    items <- sepBy1 parseInt (char ',' >> spaces)
    
    spaces >> string "Operation:" >> spaces 
    operation <- parseOperation

    spaces >> string "Test:" >> spaces >> string "divisible" >> spaces >> string "by" >> spaces
    divisibleTest <- parseInt

    spaces >> string "If" >> spaces >> string "true:" >> spaces >> string "throw to monkey" >> spaces
    onTrue <- parseInt

    spaces >> string "If" >> spaces >> string "false:" >> spaces >> string "throw to monkey" >> spaces
    onFalse <- parseInt

    return $ Monkey monkeyID items operation divisibleTest onTrue onFalse

parseOperation :: Parser (Int -> Int)
parseOperation = do
    string "new" >> spaces >> char '=' >> spaces >> string "old" >> spaces

    operator <- anyChar
    spaces

    parseIntOperation operator  <|> parseIdentityOperation operator

parseIntOperation :: Char -> Parser (Int -> Int)
parseIntOperation operator = do
    operand <- parseInt

    return $ case operator of
        '+' -> (+ operand)
        '*' -> (* operand)

parseIdentityOperation :: Char -> Parser (Int -> Int)
parseIdentityOperation operator = do
    string "old"
            
    return $ case operator of
        '+' -> (\old -> old + old)
        '*' -> (\old -> old * old)

parseInt :: Parser Int
parseInt = read <$> many1 digit