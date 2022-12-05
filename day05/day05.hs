import qualified Text.Parsec as Parsec
import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.Char (isAlphaNum)

data Instruction = Instruction {
    from :: Int,
    to :: Int,
    amount :: Int
} deriving (Show)

type InstructionParser = Parsec.Parsec String ()

main :: IO ()
main = do
    [supplyStacksStr, instructionsStr] <- splitOn "\n\n" <$> readFile "input.txt"

    let instructions = either (error . show) id $ Parsec.runParser (Parsec.many parseInstruction) () "input.txt" instructionsStr
    let supplyStacks = parseSupplyStacks supplyStacksStr

    putStrLn $ "Part 1: " ++ show (solvePart1 supplyStacks instructions)
    putStrLn $ "Part 2: " ++ show (solvePart2 supplyStacks instructions)

solvePart1 :: [[Char]] -> [Instruction] -> String
solvePart1 supplyStacks instructions = map head (foldl applyInstructionPart1 supplyStacks instructions)

solvePart2 :: [[Char]] -> [Instruction] -> String
solvePart2 supplyStacks instructions = map head (foldl applyInstructionPart2 supplyStacks instructions)

applyInstructionPart1 :: [[Char]] -> Instruction -> [[Char]]
applyInstructionPart1 supplyStacks (Instruction from to amount) = [
        if index == from then
            drop amount stack
        else if index == to then
            (reverse . take amount $ (supplyStacks !! (from - 1))) ++ stack
        else 
            stack
        | (stack, index) <- indexedStacks
    ]
    where indexedStacks = zip supplyStacks [1..]

applyInstructionPart2 :: [[Char]] -> Instruction -> [[Char]]
applyInstructionPart2 supplyStacks (Instruction from to amount) = [
        if index == from then
            drop amount stack
        else if index == to then
            take amount (supplyStacks !! (from - 1)) ++ stack
        else 
            stack
        | (stack, index) <- indexedStacks
    ]
    where indexedStacks = zip supplyStacks [1..]

parseSupplyStacks :: String -> [[Char]]
parseSupplyStacks supplyStacksStr = map init filteredInput
    where rotatedInput = transpose . lines $ supplyStacksStr
          filteredInput = filter (not . null) . map (filter isAlphaNum) $ rotatedInput

parseInstruction :: InstructionParser Instruction
parseInstruction = do
    Parsec.string "move"
    Parsec.spaces
    amount <- read <$> Parsec.many1 Parsec.digit
    Parsec.spaces

    Parsec.string "from"
    Parsec.spaces
    from <- read <$> Parsec.many1 Parsec.digit
    Parsec.spaces
    
    Parsec.string "to"
    Parsec.spaces
    to <- read <$> Parsec.many1 Parsec.digit
    Parsec.spaces

    return $ Instruction from to amount