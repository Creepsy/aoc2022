import Text.Parsec ((<?>), (<|>), many, try, string, spaces, manyTill, endOfLine, anyChar, many1, digit, space, eof)
import Text.Parsec.String (Parser, parseFromFile)
import Control.Monad (void)
import Data.Maybe (fromJust, isJust)
import Data.List (find, partition, sort)
import Data.Either (fromRight)

data FileSystem = Folder String [FileSystem] | File String Integer deriving (Show)
data FileSystemInfo = FolderInfo String | FileInfo String Integer deriving (Show)
data TerminalCommand = CDCommand String | LSCommand [FileSystemInfo] deriving (Show) 

type FileSystemZipper = (FileSystem, [(Int, FileSystem)])

main :: IO ()
main = do
    terminalCommands <- parseFromFile parseTerminalCommands "input.txt"

    let fileSystem = fileSystemFromCommands $ fromRight [] terminalCommands

    putStrLn $ "Part 1: " ++ show (solvePart1 fileSystem)
    putStrLn $ "Part 2: " ++ show (solvePart2 fileSystem)

solvePart1 :: FileSystem -> Integer
solvePart1 = sum . map getFileSystemSize . filterFileSystem (\file -> isFolder file && getFileSystemSize file < 100000)

solvePart2 :: FileSystem -> Integer
solvePart2 root = head . dropWhile (<spaceToFree) . sort . map getFileSystemSize . filterFileSystem isFolder $ root
    where rootSize = getFileSystemSize root
          spaceToFree = max 0 30000000 - (70000000 - rootSize)
          
fileSystemFromCommands :: [TerminalCommand] -> FileSystem
fileSystemFromCommands = getFileSystem . top . foldl processCommand (newZipper rootFolder)
    where rootFolder = Folder "/" []

processCommand :: FileSystemZipper -> TerminalCommand -> FileSystemZipper
processCommand zipper (CDCommand "..") = fromJust . up $ zipper
processCommand zipper (CDCommand "/") = top zipper
processCommand zipper (CDCommand name) = fromJust . down (fileIndex name . getFileSystem $ zipper) $ zipper
processCommand zipper (LSCommand contents) = modify zipper (insertIntoFolder $ map fileSystemFromInfo contents)
    where insertIntoFolder toInsert (Folder name contents) = Folder name (contents ++ toInsert)

fileSystemFromInfo :: FileSystemInfo -> FileSystem
fileSystemFromInfo (FolderInfo name) = Folder name []
fileSystemFromInfo (FileInfo name size) = File name size

fileIndex :: String -> FileSystem -> Int
fileIndex name (Folder _ contents) = snd . fromJust . find (\(file, _) -> getFileName file == name) $ indexedContents
    where getFileName file =
                case file of
                    Folder name _ -> name
                    File name _   -> name 
          indexedContents = zip contents [0..]
fileIndex _ _ = error "Tried to index a file!"

flattenFileSystem :: FileSystem -> [FileSystem]
flattenFileSystem curr@(Folder _ contents) = curr : files ++ subFolders
    where (files, folders) = partition isFile contents
          subFolders = concatMap flattenFileSystem folders
flattenFileSystem file = [file]

filterFileSystem :: (FileSystem -> Bool) -> FileSystem -> [FileSystem]
filterFileSystem predicate = filter predicate . flattenFileSystem

getFileSystemSize :: FileSystem -> Integer
getFileSystemSize (Folder _ contents) = sum . map getFileSystemSize $ contents
getFileSystemSize (File _ size) = size

isFolder :: FileSystem -> Bool
isFolder = not . isFile

isFile :: FileSystem -> Bool
isFile (Folder _ _) = False
isFile _ = True  

-- zipper

newZipper :: FileSystem -> FileSystemZipper
newZipper toZip = (toZip, [])

getFileSystem :: FileSystemZipper -> FileSystem
getFileSystem = fst

replace :: FileSystemZipper -> FileSystem -> FileSystemZipper
replace zipper newFileSystem = modify zipper (const newFileSystem)

modify :: FileSystemZipper -> (FileSystem -> FileSystem) -> FileSystemZipper
modify (curr, parents) modifyFunc = (modifyFunc curr, parents)

top :: FileSystemZipper -> FileSystemZipper
top =  fromJust . last . takeWhile isJust . iterate (>>= up) . Just

up :: FileSystemZipper -> Maybe FileSystemZipper
up (_, []) = Nothing
up (curr, (currIndex, Folder name contents):parentsR) = Just (Folder name contents', parentsR)
    where contents' = let (before, _:after) = splitAt currIndex contents in before ++ [curr] ++ after

down :: Int -> FileSystemZipper -> Maybe FileSystemZipper
down index (curr@(Folder _ contents), parents)
    | index < length contents = Just (contents !! index, (index, curr):parents)
    | otherwise               = Nothing
down _ _ = Nothing

-- parsing

parseTerminalCommands :: Parser [TerminalCommand]
parseTerminalCommands = many parseTerminalCommand

parseTerminalCommand :: Parser TerminalCommand
parseTerminalCommand = parseLSCommand <|> parseCDCommand <?> "Invalid terminal command!"

parseLSCommand :: Parser TerminalCommand
parseLSCommand = try $ do
    string "$" >> spaces >> string "ls" >> spaces
    contents <- many parseFileSystemInfo

    return $ LSCommand contents

parseCDCommand :: Parser TerminalCommand
parseCDCommand = try $ do
    string "$" >> spaces >> string "cd" >> spaces
    path <- manyTill anyChar lineEnd

    return $ CDCommand path

parseFileSystemInfo :: Parser FileSystemInfo
parseFileSystemInfo = parseFolderInfo <|> parseFileInfo <?> "Invalid ls-command output!"

parseFolderInfo :: Parser FileSystemInfo
parseFolderInfo = try $ do
    string "dir" >> spaces
    folderName <- manyTill anyChar lineEnd

    return $ FolderInfo folderName

parseFileInfo :: Parser FileSystemInfo
parseFileInfo = try $ do
    fileSize <- read <$> many1 digit
    spaces
    fileName <- manyTill anyChar lineEnd

    return $ FileInfo fileName fileSize

spaces1 :: Parser ()
spaces1 = void $ many1 space 

lineEnd :: Parser ()
lineEnd = spaces1 <|> eof