import Data.List.Split (divvy)
import Data.List (find, nub)
import Data.Maybe (fromJust)
import Text.PrettyPrint.HughesPJClass (first)

main :: IO ()
main = do
    signal <- readFile "input.txt"
    
    putStrLn $ "Part 1: " ++ show (firstUniqueSequence signal 4)
    putStrLn $ "Part 2: " ++ show (firstUniqueSequence signal 14)

firstUniqueSequence :: String -> Int -> Int
firstUniqueSequence signal length = snd . fromJust . find (\(seg, _) -> seg == nub seg) $ indexedSegments
    where segments = divvy length 1 signal
          indexedSegments = zip segments [length..]