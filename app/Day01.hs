{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import qualified Control.Exception as CE
import           Control.Monad     (guard)
import           Data.Char         (isDigit)
import qualified Data.List
import           Data.Maybe        (isJust)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector       as DV

parseLine1 :: LT.Text -> Int
parseLine1 line = read [firstDigit, lastDigit]
    where
        digits = filter isDigit $ LT.unpack line
        firstDigit : _ = digits
        lastDigit = last digits

-- >>> parseLine1 $ LT.pack "a1b2c3d4e5f"
-- 15

parseToken2 :: LT.Text -> Maybe Int
parseToken2 suffix = fst <$> Data.List.uncons matches
    where
        options = [(["0"], 0),
                   (["1", "one"], 1),
                   (["2", "two"], 2),
                   (["3", "three"], 3),
                   (["4", "four"], 4),
                   (["5", "five"], 5),
                   (["6", "six"], 6),
                   (["7", "seven"], 7),
                   (["8", "eight"], 8),
                   (["9", "nine"], 9)]
        matches :: [Int]
        matches = do
            (variants, result) <- options
            guard $ any (`LT.isPrefixOf` suffix) variants
            return result


-- >>> parseToken2 "three"
-- Just 3

suffixes :: LT.Text -> [LT.Text]
suffixes text = maybe [] ((text :) . suffixes . snd) $ LT.uncons text
-- >>> suffixes $ LT.pack "foobar"
-- ["foobar","oobar","obar","bar","ar","r"]


parseLine2 :: LT.Text -> Int
parseLine2 line = 10 * firstDigit + lastDigit
    where
        maybeDigits = map parseToken2 $ suffixes line
        digits = filter isJust maybeDigits
        Just firstDigit = head digits
        Just lastDigit = last digits

-- >>> parseLine2 $ LT.pack "eight5oneights"
-- 88

solve :: String -> IO (Either CE.SomeException Int, Either CE.SomeException Int)
solve inputFilename = do
    inputText <- LTIO.readFile inputFilename
    let lines = DV.fromList $ filter (not . LT.null) $ LT.lines inputText
    solution1 <- CE.try . CE.evaluate . DV.sum . DV.map parseLine1 $ lines
    solution2 <- CE.try . CE.evaluate . DV.sum . DV.map parseLine2 $ lines

    return (solution1, solution2)
-- >>> solve "inputs/sample/01.1.txt"
-- (Right 142,Right 142)

-- >>> solve "inputs/sample/01.2.txt"
-- (Left /workspaces/advent-2023/app/Day01.hs:18:9-31: Non-exhaustive patterns in firstDigit : _
-- ,Right 281)

-- >>> solve "inputs/real/01.txt"
-- (Right 55971,Right 54719)
