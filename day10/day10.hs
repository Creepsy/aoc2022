import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)

data Instruction = AddX Int | NoOp deriving (Show)

main :: IO ()
main = do
    instructions <- map parseInstruction . lines <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show (solvePart1 instructions)
    putStrLn $ "Part 2:\n" ++ solvePart2 instructions

solvePart1 :: [Instruction] -> Int
solvePart1 instructions = sum . zipWith (*) measurementCycles . map (cycleValues !!) $ cylceIndices
    where cycleValues = executeProgram instructions 
          measurementCycles = [20, 60, 100, 140, 180, 220]
          cylceIndices = map (subtract 1) measurementCycles
 
solvePart2 :: [Instruction] -> String
solvePart2 instructions = unlines . chunksOf 40 . zipWith renderPixel cycleValues $ cycles
    where cycleValues = init . executeProgram $ instructions
          cycles = [0..]
          currentHorizontalIndex cycle = cycle `mod` 40
          spriteVisible spritePos cycle = abs (currentHorizontalIndex cycle - spritePos) < 2
          renderPixel spritePos cycle = if spriteVisible spritePos cycle then '#' else '.'

executeProgram :: [Instruction] -> [Int]
executeProgram = reverse . foldl (\history@(currX:_) instr -> executeInstruction instr currX ++ history) [initialX]
    where initialX = 1

executeInstruction :: Instruction -> Int -> [Int]
executeInstruction (AddX toAdd) currX = [currX + toAdd, currX]
executeInstruction NoOp currX = [currX]

parseInstruction :: String -> Instruction
parseInstruction toParse
    | "noop" `isPrefixOf` toParse = NoOp
    | "addx" `isPrefixOf` toParse = AddX (head args)
    where args = map read . drop 1 . words $ toParse