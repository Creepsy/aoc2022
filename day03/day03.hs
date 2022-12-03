import Data.Char (ord, isLower, isUpper)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)

type Backpack = (String, String)

parseBackpack :: String -> Backpack
parseBackpack toParse = splitAt (length toParse `div` 2) toParse 

itemPriority :: Char -> Int
itemPriority c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = error "Found invalid item in backpack!"

findUnsortedItem :: Backpack -> Char
findUnsortedItem (fCompartment, sCompartment) = 
    if length duplicateItems /= 1 then
        error "Found multiple unsorted items!"
    else 
        head duplicateItems
    where duplicateItems = nub $ fCompartment `intersect` sCompartment

findBadge :: [Backpack] -> Char
findBadge group = 
    if length badges /= 1 then
        error "Found multiple badges!"
    else
        head badges
    where badges = nub . foldl1 intersect $ group'
          group' = map (uncurry (++)) group

solvePart1 :: [Backpack] -> Int
solvePart1 = sum . map (itemPriority . findUnsortedItem)

solvePart2 :: [[Backpack]] -> Int
solvePart2 = sum . map (itemPriority . findBadge)

main :: IO ()
main = do
    backpacks <- map parseBackpack . lines <$> readFile "input.txt"
    let groups = chunksOf 3 backpacks

    putStrLn $ "Part 1: " ++ show (solvePart1 backpacks)
    putStrLn $ "Part 2: " ++ show (solvePart2 groups)