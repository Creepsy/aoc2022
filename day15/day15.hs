import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec (string, many1, digit, (<|>), char, sepBy, spaces)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.List (sortBy, minimumBy, sort)
import Data.Function (on)
import Control.Monad (foldM)
import Data.List.Split (divvy)
import Data.Maybe (isJust)

type Position = (Int, Int)
type Interval = (Int, Int)

-- Part 2 really slow and ugly; maybe improve later

main :: IO ()
main = do
    beaconSensorPairs <- parseFromFile (sepBy parseSensorBeaconPair spaces) "input.txt"

    if isLeft beaconSensorPairs then
        print $ fromLeft undefined beaconSensorPairs
    else do
        let beaconSensorPairs' = fromRight [] beaconSensorPairs
            sortedBeaconSensorPairs = sortBy (compare `on` fst) beaconSensorPairs'
            
        putStrLn $ "Part 1: " ++ show (solvePart1 sortedBeaconSensorPairs)
        putStrLn $ "Part 2: " ++ show (solvePart2 sortedBeaconSensorPairs)

solvePart1 :: [(Position, Position)] -> Int
solvePart1 = sum . map intervalLength . lineCoverage 2000000

solvePart2 :: [(Position, Position)] -> Int
solvePart2 beaconSensorPairs = tuningFrequency $ head allUncoveredPositions''
    where possibleLines = [0..4000000]
          lineCoverages = filter (\line -> length line > 1) . map (`lineCoverage` beaconSensorPairs) $ possibleLines
          lineCoverages' = map sort lineCoverages
          allUncoveredPositions = filter (isJust . snd) . zip possibleLines . map findUncoveredPosition $ lineCoverages'
          allUncoveredPositions' =  map (\(posY, Just posX) -> (posX, posY)) allUncoveredPositions
          allUncoveredPositions'' = filter (`notElem` beacons) allUncoveredPositions'
          findUncoveredPosition = foldl updateIntervalGaps Nothing . divvy 2 1
          updateIntervalGaps currentGap [first, second] = if isJust currentGap then currentGap else gapBetweenIntervals first second
          gapBetweenIntervals (_, firstEnd) (secondStart, _) = if firstEnd + 1 == secondStart then Nothing else Just $ firstEnd + 1
          beacons = map snd beaconSensorPairs
          tuningFrequency (posX, posY) = posX * 4000000 + posY

lineCoverage :: Int -> [(Position, Position)] -> [Interval]
lineCoverage lineNumber = foldl (applyBeaconSensorPairToLineState lineNumber) []

applyBeaconSensorPairToLineState :: Int -> [Interval] -> (Position, Position) -> [Interval]
applyBeaconSensorPairToLineState lineNumber intervals (sensorPos@(sensorX, sensorY), beaconPos@(beaconX, beaconY)) =
    if doesAffectLine then
        if beaconY == lineNumber then removeInterval (beaconX, beaconX) newIntervals else newIntervals
    else
        intervals
    where lineDelta = manhattanDistance sensorPos beaconPos - manhattanDistance sensorPos (sensorX, lineNumber)
          doesAffectLine = lineDelta >= 0
          leftBound = sensorX - lineDelta
          rightBound = sensorX + lineDelta
          newIntervals = insertInterval (leftBound, rightBound) intervals

manhattanDistance :: Position -> Position -> Int
manhattanDistance (firstX, firstY) (secondX, secondY) = abs (firstX - secondX) + abs(firstY - secondY)

insertInterval :: Interval -> [Interval] -> [Interval]
insertInterval toInsert intervals = toInsert : removeInterval toInsert intervals 

removeInterval :: Interval -> [Interval] -> [Interval]
removeInterval toRemove = concatMap (\interval -> maybe [interval] (remove interval) $ intersect interval toRemove)
    where remove operand@(operandStart, operandEnd) toRemove@(toRemoveStart, toRemoveEnd)
            | toRemove == operand = []
            | toRemoveEnd == operandEnd = [(operandStart, toRemoveStart - 1)]
            | toRemoveStart == operandStart = [(toRemoveEnd + 1, operandEnd)]
            | otherwise = [(operandStart, toRemoveStart - 1), (toRemoveEnd + 1, operandEnd)]

intervalLength :: Interval -> Int
intervalLength (start, end) = end - start + 1

intersect :: Interval -> Interval -> Maybe Interval
intersect (fStart, fEnd) (sStart, sEnd)
    | fStart > sEnd || sStart > fEnd = Nothing
    | otherwise = Just (max fStart sStart, min fEnd sEnd)

parseSensorBeaconPair :: Parser (Position, Position)
parseSensorBeaconPair = do
    string "Sensor at x="
    sensorX <- parseInt
    
    string ", y="
    sensorY <- parseInt

    string ": closest beacon is at x="
    beaconX <- parseInt

    string ", y="
    beaconY <- parseInt

    return ((sensorX, sensorY), (beaconX, beaconY))

parseInt :: Parser Int
parseInt = unsignedInt <|> (char '-' >> negate <$> unsignedInt) 
    where unsignedInt = read <$> many1 digit 