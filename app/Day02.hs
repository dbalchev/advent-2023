{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell    #-}

module Day02 where
import           Data.Vector          (Vector, fromList, toList)

import           Control.Lens         (Lens', makeLenses, (.~), (^.))
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Lazy.IO    as LTIO
import           Text.Parsec          (choice, parse, sepBy, spaces, string,
                                       try)
import qualified Text.Parsec.Language as TPL
import qualified Text.Parsec.Token    as TPT

data CubeCount = CubeCount {_redCount :: Int, _blueCount :: Int, _greenCount :: Int}
    deriving Show
makeLenses ''CubeCount

zeroCubeCount = CubeCount 0 0 0
cubeCountLens :: [Lens' CubeCount Int]
cubeCountLens = [redCount, blueCount, greenCount]

type Game = (Int, Vector CubeCount)
intParser = fromEnum <$> TPT.integer TPL.haskell

singleCountParser countLens literal = do
    count <- intParser
    spaces
    string literal
    return $ countLens .~ count $ zeroCubeCount

-- >>> parse (singleCountParser redCount "red") "" "5 red"
-- Right (CubeCount {_redCount = 5, _blueCount = 0, _greenCount = 0})

combineRound :: [CubeCount] -> CubeCount
combineRound singleCounts = foldl (flip lensSet) zeroCubeCount cubeCountLens
    where
        lensSet :: Lens' CubeCount Int -> CubeCount -> CubeCount
        lensSet lens = lens .~ maximum (map (^. lens) singleCounts)

roundParser = do
    let singleParser = choice [
                                 try $ singleCountParser redCount "red",
                                 try $ singleCountParser blueCount "blue",
                                 try $ singleCountParser greenCount "green"
                              ]
    combineRound <$> sepBy singleParser (string ", ")


-- >>> parse roundParser "" "5 red, 2 green, 3 blue"
-- Right (CubeCount {_redCount = 5, _blueCount = 3, _greenCount = 2})

gameParser = do
    string "Game"
    spaces
    gameIndex <- intParser
    string ":"
    rounds <- sepBy roundParser (string "; ")
    return (gameIndex, fromList rounds)

-- >>> parse gameParser "" "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
-- Right (2,[CubeCount {_redCount = 0, _blueCount = 1, _greenCount = 2},CubeCount {_redCount = 1, _blueCount = 4, _greenCount = 3},CubeCount {_redCount = 0, _blueCount = 1, _greenCount = 1}])

readInput inputFilename = do
    inputText <- LTIO.readFile inputFilename
    let lines = filter (not . LT.null) $ LT.lines inputText
    let parsedLines = map (parse gameParser "" . LT.unpack) lines
    let Right parsedGames = sequenceA parsedLines
    return parsedGames

isPossible cubeCount = all (\(lens, count) -> (cubeCount ^. lens) <= count) checks
    where
        checks = [(redCount, 12), (greenCount, 13), (blueCount, 14)]

powerOf :: [CubeCount] -> Int
powerOf rounds = product $ map minFor cubeCountLens
    where
        minFor :: Lens' CubeCount Int -> Int
        minFor lens = maximum . map (^. lens) $ rounds


solve inputFilename = do
    games <- readInput inputFilename
    let solution1 = sum $ map fst $ filter (all isPossible . snd) games
    let solution2 = sum . map (powerOf . toList . snd) $ games
    return (solution1, solution2)

-- >>> solve "inputs/sample/02.txt"
-- (8,2286)

-- >>> solve "inputs/real/02.txt"
-- (2447,56322)
