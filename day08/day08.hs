import Data.Char (digitToInt)

type TreeGrid = ([[Int]], Int , Int)

-- Arrays would probably lead to a drastically increased performance here, but I am lazy ;D 

main :: IO ()
main = do
    grid <- map (map digitToInt) . lines <$> readFile "input.txt"
    let gridWidth = length $ head grid
        gridHeight = length grid
        treeGrid = (grid, gridWidth, gridHeight)

    putStrLn $ "Part 1: " ++ show (solvePart1 treeGrid)
    putStrLn $ "Part 2: " ++ show (solvePart2 treeGrid)

solvePart1 :: TreeGrid -> Int
solvePart1 = length . filter id . concat . visibleTreesGrid

solvePart2 :: TreeGrid -> Int
solvePart2 tG@(grid, width, height) = maximum . concat $ [[calculateScenicScore tG x y | x <- [0..width - 1]] | y <- [0..height - 1]]

calculateScenicScore :: TreeGrid -> Int -> Int -> Int
calculateScenicScore tG@(grid, width, height) xPos yPos = product . map visibleTrees $ surroundings
    where surroundings = surroundingTrees tG xPos yPos
          treeHeight = grid !! yPos !! xPos
          visibleTrees trees = let (visible, notVisible) = span (< treeHeight) trees in length visible + fromEnum (not . null $ notVisible)

surroundingTrees :: TreeGrid -> Int -> Int -> [[Int]]
surroundingTrees tG@(grid, width, height) xPos yPos = [reverse rowBefore, rowAfter, reverse columnBefore, columnAfter]
    where splitConsume index list = (take index list, drop (index + 1) list)
          (rowBefore, rowAfter) = splitConsume xPos (row yPos tG)
          (columnBefore, columnAfter) = splitConsume yPos (column xPos tG)

visibleTreesGrid :: TreeGrid -> [[Bool]]
visibleTreesGrid tG@(grid, width, height) = [[isTreeVisible tG x y | x <- [0..width - 1]] | y <- [0..height - 1]]

isTreeVisible :: TreeGrid -> Int -> Int -> Bool
isTreeVisible tG@(grid, width, height) xPos yPos = any ((< treeHeight) . maximumOr (-1)) surroundings
    where surroundings = surroundingTrees tG xPos yPos
          treeHeight = grid !! yPos !! xPos

maximumOr :: Ord a => a-> [a] -> a
maximumOr val[] = val
maximumOr _ list = maximum list

row :: Int -> TreeGrid -> [Int]
row index (grid, _, _)= grid !! index

column :: Int ->TreeGrid -> [Int]
column index (grid, _, _) = map (!!index) grid