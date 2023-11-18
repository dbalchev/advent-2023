{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import           Data.Char                    (isSpace)
import qualified Data.Text.Lazy               as LT
import           Data.Text.Lazy.Read          (decimal, signed)
import qualified Data.Vector                  as DV
import           Data.Vector.Algorithms.Merge (sort)
import           Text.Parsec                  (many1, newline, optional, parse,
                                               satisfy)
import           Text.Parsec.Text.Lazy        (Parser, parseFromFile)

myRead word = x
    where
        Right (x, _) = signed decimal word

withOptionalNewLine :: Parser a -> Parser a
withOptionalNewLine parser = do
    parsed <- parser
    optional newline
    return parsed


inputParser = DV.fromList <$> many1 anElf
    where
        singleLine = myRead . LT.pack <$> withOptionalNewLine (many1 $ satisfy (not . isSpace))
        anElf = withOptionalNewLine $ DV.fromList <$> many1 singleLine
-- >>> parse inputParser "" "1234\n4567\n\n12\n34"
-- Right [[1234,4567],[12,34]]

solve :: LT.Text -> IO (Int, Int)
solve inputFilename = do
    Right parsedInput <- parseFromFile inputParser $ LT.unpack inputFilename
    let sums = DV.map DV.sum parsedInput
    let solution1 = DV.maximum sums
    print solution1
    let sortedSums = DV.modify sort sums
    let solution2 = DV.sum $ DV.slice (DV.length sortedSums - 3) 3 sortedSums
    print solution2
    return (solution1, solution2)

-- >>> solve "inputs/sample/01.txt"
-- (24000,45000)

-- >>> solve "inputs/real/01.txt"
-- (67658,200158)
