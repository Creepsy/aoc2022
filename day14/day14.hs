import Data.List.Split (splitOn, chunksOf)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Control.Monad (forM_, void, when)
import Control.Monad.Loops (iterateWhile, whileM_)

type Position = (Int, Int)
type RockStructure = [Position]
type IOVector2D a = ((Int, Int), VM.IOVector a)

main :: IO ()
main = do
    rockStructures <- map parseRockStructure . lines <$> readFile "input.txt"
    
    let ((minX, minY), (_, maxY)) = getBounds . concat $ rockStructures
        heightPart2 = maxY - minY + 3

    part1Solution <- solvePart1 rockStructures (500 - minX, 0)
    part2Solution <- solvePart2 rockStructures (500 - minX + heightPart2, 0)

    putStrLn $ "Part 1: " ++ show part1Solution
    putStrLn $ "Part 2: " ++ show part2Solution

solvePart1 :: [RockStructure] -> Position -> IO Int
solvePart1 rockStructures dropPosition = do
    structureMap2D <- rockStructureMap rockStructures False
    iterateWhile id $ dropSand structureMap2D dropPosition

    V.length . V.filter (=='o') <$> V.freeze (snd structureMap2D)

solvePart2 :: [RockStructure] -> Position -> IO Int
solvePart2 rockStructures dropPosition = do
    structureMap2D <- rockStructureMap rockStructures True
    whileM_ ((/='o') <$> VM.read (snd structureMap2D) (index2D (fst structureMap2D) dropPosition)) $ dropSand structureMap2D dropPosition

    V.length . V.filter (=='o') <$> V.freeze (snd structureMap2D)


dropSand :: IOVector2D Char -> Position -> IO Bool
dropSand structureMap2D@(bounds, structureMap) pos@(posX, posY)
    | outOfBounds structureMap2D pos = return False
    | otherwise     = do
        nextLayer <- sequence [isBlocked structureMap2D (posX + offX, posY + 1) | offX <- [-1..1]]
        
        case nextLayer of
            [_, False, _]       -> dropSand structureMap2D (posX, posY + 1)
            [False, True, _]    -> dropSand structureMap2D (posX - 1, posY + 1)
            [True, True, False] -> dropSand structureMap2D (posX + 1, posY + 1)
            [True, True, True]  -> VM.write structureMap (index2D bounds pos) 'o' >> return True
   
isBlocked :: IOVector2D Char -> Position -> IO Bool
isBlocked structureMap2D@(bounds, structureMap) pos = do 
    if outOfBounds structureMap2D pos then
        return False
    else 
        (/='.') <$> VM.read structureMap (index2D bounds pos)

outOfBounds :: IOVector2D Char -> Position -> Bool
outOfBounds ((width, height), _) (posX, posY) = posX < 0 || posX >= width || posY < 0 || posY >= height

printRockStructureMap :: IOVector2D Char -> IO ()
printRockStructureMap ((width, height), structureMap) = putStrLn . unlines . chunksOf width . V.toList =<< V.freeze structureMap

rockStructureMap :: [RockStructure] -> Bool -> IO (IOVector2D Char)
rockStructureMap rockStructures withBottom = do
    let ((minX, minY), (maxX, maxY)) = getBounds . concat $ rockStructures
        height = maxY + 1 + if withBottom then 2 else 0
        width = maxX - minX + 1 + if withBottom then 2 * height else 0
        minX' = if withBottom then minX - height else minX
        rockStructures' = map (normalizeRockStructure (minX', minY)) rockStructures

    structureMap <- VM.replicate (width * height) '.'
    let structureMap2D = ((width, height), structureMap)

    forM_ rockStructures' (createRockStructure structureMap2D)
        
    when withBottom $ forM_ [0..width - 1] (\posX -> VM.write structureMap (index2D (width, height) (posX, height - 1)) '#')
 
    return structureMap2D

createRockStructure :: IOVector2D Char -> RockStructure -> IO ()
createRockStructure structureMap2D rockStructure = forM_ walls (uncurry $ insertRockWall structureMap2D)
    where walls = zip rockStructure (drop 1 rockStructure)

insertRockWall :: IOVector2D Char -> Position -> Position -> IO ()
insertRockWall (bounds, structureMap) (startX, startY) (endX, endY) = do
    let minX = min startX endX
        maxX = max startX endX
        minY = min startY endY
        maxY = max startY endY

    void . sequence $ [VM.write structureMap (index2D bounds (posX, posY)) '#' | posX <- [minX..maxX], posY <- [minY..maxY]]

normalizeRockStructure :: (Int, Int) -> RockStructure -> RockStructure
normalizeRockStructure (minX, minY) = map (\(posX, posY) -> (posX - minX, posY - minY))

getBounds :: [Position] -> (Position, Position)
getBounds positions = ((minX, 0), (maxX, maxY))
    where minX = minimum . map fst $ positions
          maxX = maximum . map fst $ positions
          maxY = maximum . map snd $ positions

parseRockStructure :: String -> RockStructure
parseRockStructure rockStructureStr = cornerPoints
    where corners = splitOn " -> " rockStructureStr
          asTuple [a, b] = (a, b)
          cornerPoints = map (asTuple . map read . splitOn ",") corners

index2D :: (Int, Int) -> Position -> Int
index2D (width, _) (posX, posY) = posY * width + posX