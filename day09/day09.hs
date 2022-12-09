import qualified Data.Set as Set

type Position = (Int, Int)
type Instruction = (Char, Int)
type Rope = [Position]

main :: IO ()
main = do
    instructions <- map (\(code:' ':lengthStr) -> (code, read lengthStr)) . lines <$> readFile "input.txt" :: IO [Instruction]

    putStrLn $ "Part 1: " ++ show (solvePart1 instructions) 
    putStrLn $ "Part 2: " ++ show (solvePart2 instructions) 

solvePart1 :: [Instruction] -> Int
solvePart1 = countUniqueTailPositions . simulateRopeMovements (createRope 2)

solvePart2 :: [Instruction] -> Int
solvePart2 = countUniqueTailPositions . simulateRopeMovements (createRope 10)

countUniqueTailPositions :: [Rope] -> Int
countUniqueTailPositions = Set.size . Set.fromList . map last

simulateRopeMovements :: Rope -> [Instruction] -> [Rope]
simulateRopeMovements rope = foldl (\(curr:acc) instruction -> executeInstruction instruction curr ++ acc) [rope]

createRope :: Int -> Rope
createRope length = replicate length (0, 0)

executeInstruction :: Instruction -> Rope -> [Rope]
executeInstruction (code, amount) = reverse . take (amount + 1) . iterate (stepInstructionCode code)

stepInstructionCode :: Char -> Rope -> Rope
stepInstructionCode code ((headX, headY):ropeR) = newHead : moveRopeSegments ropeR newHead
    where newHead = case code of
                        'R' -> (headX + 1, headY)
                        'L' -> (headX - 1, headY)
                        'U' -> (headX, headY + 1)
                        'D' -> (headX, headY - 1)
                        _   -> error "Invalid instruction code!"

moveRopeSegments :: Rope -> Position -> Rope
moveRopeSegments [] _ = []
moveRopeSegments rope@(curr@(currX, currY):ropeR) prev@(prevX, prevY)
    | isNextTo curr prev = rope
    | otherwise = newPosition : moveRopeSegments ropeR newPosition
    where (deltaX, deltaY) = positionDelta curr prev
          newPosition = (currX + signum deltaX, currY + signum deltaY) 

isNextTo :: Position -> Position -> Bool
isNextTo first second = abs deltaX < 2 && abs deltaY < 2
    where (deltaX, deltaY) = positionDelta first second

positionDelta :: Position -> Position -> Position
positionDelta (firstX, firstY) (secondX, secondY) = (deltaX, deltaY)
    where deltaX = secondX - firstX
          deltaY = secondY - firstY