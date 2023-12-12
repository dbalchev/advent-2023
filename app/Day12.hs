{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V

import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST (runST)
import Data.List (intersperse, intercalate)

parseLine :: T.Text -> (V.Vector Char, V.Vector Int)
parseLine line = (V.fromList . T.unpack $ brokenMap, brokenGroupSizes)
    where
        [brokenMap, brokenGroupSizesTxt] = T.words line
        brokenGroupSizes = V.fromList $ read . T.unpack <$> T.splitOn "," brokenGroupSizesTxt

htFix f = do
    memo <- HT.new
    let g arg = do
        memoed <- HT.lookup memo arg
        let computeAndSave = do
            result <- f g arg
            HT.insert memo arg result
            return result
        maybe computeAndSave return memoed
    return g

solve1 brokenMap brokenGroupSizes
    = runST $ do
        memoedGo <-htFix go
        memoedGo (V.length brokenMap - 1, V.length brokenGroupSizes - 1, -1)
        where
            go rGo (i, j, brokenRemainingInGroup) = pureGo rGo (i, j, brokenRemainingInGroup) (brokenMap V.! i)
            pureGo _ (-1, -1, 0) _  = return 1
            pureGo _ (-1, -1, -1) _ = return 1
            pureGo _ (-1, _, _) _   = return 0
            pureGo rGo state@(i, j, brokenRemainingInGroup) currentChar
                | currentChar == '.' && brokenRemainingInGroup > 0              = return 0
                | currentChar == '.' && brokenRemainingInGroup < 1              = rGo (i - 1, j, -1)
                | currentChar == '#' && brokenRemainingInGroup > 0              = rGo (i - 1, j, brokenRemainingInGroup - 1)
                | currentChar == '#' && brokenRemainingInGroup == 0             = return 0
                | currentChar == '#' && brokenRemainingInGroup == -1 && j == -1 = return 0
                | currentChar == '#' && brokenRemainingInGroup == -1            = rGo (i - 1, j - 1, brokenGroupSizes V.! j - 1)
                | currentChar == '?' = (+) <$> pureGo rGo state '.' <*> pureGo rGo state '#'


repeat5 v = V.concatMap (const v) (V.replicate 5 ())

-- >>> repeat5 . V.fromList $ [1, 2, 3]
-- [1,2,3,1,2,3,1,2,3,1,2,3,1,2,3]

repeat5sep = V.fromList . intercalate "?" . replicate 5 . V.toList

-- >>> repeat5sep $ V.fromList ".#"
-- ".#?.#?.#?.#?.#"

solve2 brokenMap brokenGroupSizes = solve1 rbm rbgs
    where
        rbm = repeat5sep brokenMap
        rbgs = repeat5 brokenGroupSizes

solve inputFilename = do
    input <- (parseLine <$>) . T.lines <$> T.readFile inputFilename
    let solution1 = sum $ uncurry solve1 <$> input
    let solution2 = sum $ uncurry solve2 <$> input
    return (solution1, solution2)

-- >>> solve "inputs/sample/12.txt"
-- (21,525152)

-- >>> solve "inputs/real/12.txt"
-- (7251,2128386729962)
