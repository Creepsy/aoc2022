import Data.List.Split (splitOn, chunksOf)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (many1, many, char, digit, lookAhead, (<|>), (<?>), sepBy, spaces)
import Data.Either (fromLeft, isLeft, fromRight)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data PacketElement = List [PacketElement] | Value Int deriving (Eq, Show)
type Packet = [PacketElement]

instance Ord PacketElement where
    (<=) :: PacketElement -> PacketElement -> Bool
    (List firstElements) <= (List secondElements) = firstElements <= secondElements
    (Value firstValue) <= (Value secondValue) = firstValue <= secondValue
    (List firstElements) <= second = firstElements <= [second]
    first <= (List secondElements) = [first] <= secondElements


main :: IO ()
main = do
    packets <- parseFromFile (sepBy parsePacket spaces) "input.txt"

    if isLeft packets then
        print $ fromLeft undefined packets
    else do
        let packets' = fromRight [] packets
            packetPairs = map (\[first, second] -> (first, second)) . chunksOf 2 $ packets'

        putStrLn $ "Part 1: " ++ show (solvePart1 packetPairs)
        putStrLn $ "Part 2: " ++ show (solvePart2 packets')

-- parsing

solvePart1 :: [(Packet, Packet)] -> Int
solvePart1 = sum . map fst . filter ((==True) . snd) . zip [1..] . map (uncurry (<=)) 

solvePart2 :: [Packet] -> Int
solvePart2 packets = divider2Index * divider6Index
    where dividerPacket2 = [List [Value 2]]
          dividerPacket6 = [List [Value 6]]
          packets' = dividerPacket2 : dividerPacket6 : packets
          sortedPackets = sort packets'
          divider2Index = fromJust (dividerPacket2 `elemIndex` sortedPackets) + 1
          divider6Index = fromJust (dividerPacket6 `elemIndex` sortedPackets) + 1

parsePacket :: Parser Packet
parsePacket = do
    char '['
    packetData <- sepBy parsePacketElement (char ',' >> spaces)
    char ']'

    return packetData

parsePacketElement :: Parser PacketElement
parsePacketElement = do
    lookAhead (char '[')
    List <$> parsePacket

    <|> Value <$> parseInt
    <?> "Invalid packet element!"


parseInt :: Parser Int
parseInt = read <$> many1 digit