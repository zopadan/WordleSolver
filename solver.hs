import Data.List
import System.IO
import System.Environment
import Text.Read (Lexeme(String))
import Text.XHtml (color)
import GHC.Float (word2Double)

------------------------------------------------------------------------------
-- Validating methods --

data WordInfo = WordInfo String String Int
    deriving Show

isColor :: String -> Bool
isColor color
    | color == "g" = True
    | color == "y" = True
    | otherwise = False

stringToInt :: String -> Int
stringToInt = read 

buildWordInfoFromStr :: String -> String -> String -> Maybe WordInfo
buildWordInfoFromStr letter color index 
    | isColor color && length letter == 1 = Just (WordInfo letter color (stringToInt index))
    | otherwise     = Nothing

parseLine :: [String] -> [Maybe WordInfo]
parseLine (x:y:z:zs) = buildWordInfoFromStr x y z:parseLine zs
parseLine _ = []

-----------------------------------------------------------------------------
-- Processing Methods --

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

stringToChar :: String -> Char
stringToChar [c] = c
stringToChar _   = ' '  -- Forcing validateYellow to fail by passing empty char if the letter is not valid

validateGreen :: String -> Int -> String -> Bool
validateGreen letter index word
    | [word !! (index - 1)] == letter = True
    | otherwise = False

validateYellow :: String -> Int -> String -> Bool
validateYellow letter index word
    | [word !! (index - 1)] /= letter && trueLetter `elem` word = True
    | otherwise = False
    where trueLetter = stringToChar letter

checkWord :: WordInfo -> String -> Bool
checkWord (WordInfo letter color index) word 
    | color == "g" = validateGreen letter index word
    | color == "y" = validateYellow letter index word
    | otherwise = False

trimValidWords :: Maybe WordInfo -> [String] -> [String]
trimValidWords wInfo words = ["Dummy"]

processResults :: [Maybe WordInfo] -> [String] -> [String]
processResults (x:xs) validWords = processResults xs (trimValidWords x validWords)

-- Conditions for popping a word
    -- greens don't match
    -- yellow is a match
    -- yellow letter missing

---------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "List the known letters followed by color and index e.g. L g 1"
    inputInfo <- getArgs
    putStrLn "Parsing "
    let inputWordInfo = parseLine inputInfo
    let results = processResults inputWordInfo
    print inputWordInfo