import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.List (elemIndex, sortBy, nub)
import Data.Char (ord)
import Data.Function (on)
import qualified Data.Map as Map

type HeightMap = ([String], Int, Int) 
type Position = (Int, Int)
type Path = [Position]

main :: IO ()
main = do
    heightMap <- lines <$> readFile "input.txt"

    let width = length . head $ heightMap
        height = length heightMap
        heightMap' = (heightMap, width, height)
        

    putStrLn $ "Part 1: " ++ show (solvePart1 heightMap')
    putStrLn $ "Part 2: " ++ show (solvePart2 heightMap')

solvePart1 :: HeightMap -> Int
solvePart1 heightMap = length (fromJust $ traceReversedPath connections start end) - 1
    where start = head . findPositions 'S' $ heightMap
          end = head . findPositions 'E' $ heightMap
          connections = floodfill stepPossible heightMap [start] Map.empty

solvePart2 :: HeightMap -> Int
solvePart2 heightMap = minimum . map (subtract 1 . length) $ paths
    where starts = findPositions 'a' heightMap
          end = head . findPositions 'E' $ heightMap
          connections = floodfill (flip . stepPossible) heightMap [end] Map.empty
          paths = mapMaybe (traceReversedPath connections end) starts

traceReversedPath :: Map.Map Position Position -> Position -> Position -> Maybe Path
traceReversedPath connections from to
    | to `Map.notMember` connections = Nothing
    | from == to                     = Just [to]
    | otherwise                      = let prev = fromJust . Map.lookup to $ connections in (to :) <$> traceReversedPath connections from prev

floodfill :: (HeightMap -> Position -> Position -> Bool) -> HeightMap -> [Position] -> Map.Map Position Position -> Map.Map Position Position
floodfill stepFunc _ [] visited = visited
floodfill stepFunc heightMap borders visited = floodfill stepFunc heightMap borders' . foldl (\acc pos -> makeAllVisited pos acc (newNeighbours pos)) visited $ borders
    where isVisited pos = pos `Map.member` visited
          newNeighbours pos = filter (not . isVisited) . filter (stepFunc heightMap pos) . neighbours heightMap $ pos
          borders' = nub . concatMap newNeighbours $ borders
          makeAllVisited from = foldl (\acc target -> Map.insert target from acc)

neighbours :: HeightMap -> Position -> [Position]
neighbours (heightMap, width, height) center@(posX, posY) = [pos | (offX, offY) <- offsets, let pos = (posX + offX, posY + offY), isValidNeighbour pos]
    where offsets = [(0, 1), (1, 0), (0, -1), (-1, 0)]
          isValidNeighbour pos@(posX, posY) = posX >= 0 && posY >= 0 && posX < width && posY < height

stepPossible :: HeightMap -> Position -> Position -> Bool
stepPossible heightMap from to = fromHeight + 1 >= toHeight 
    where fromHeight = getHeight heightMap from
          toHeight = getHeight heightMap to

getHeight :: HeightMap -> Position -> Int
getHeight (heightMap, _, _) (posX, posY)
    | posChar == 'S' = 0
    | posChar == 'E' = 25
    | otherwise      = ord posChar - ord 'a'
    where posChar = heightMap !! posY !! posX

findPositions :: Char -> HeightMap -> [Position]
findPositions toFind (grid, width, height) = [(posX, posY) | posX <- [0..width - 1], posY <- [0..height - 1], grid !! posY !! posX == toFind]