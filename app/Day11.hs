{-# LANGUAGE TupleSections #-}
module Day11 where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V
import AocParser (readCharMap)
import Control.Monad (guard)
import Data.List (tails, sort)

pairs list = do
    t <- tails list
    guard $ (not . null) t
    let (x : xs) = t
    map (x,) xs

-- >>> pairs "abcd"
-- [('a','b'),('a','c'),('a','d'),('b','c'),('b','d'),('c','d')]

dOfIsEmpty :: Bool -> Int
dOfIsEmpty False = 2
dOfIsEmpty True  = 1

distanceAxis isEmpty start end
    = sum $ (\i -> dOfIsEmpty $ isEmpty V.! i) <$> [a+1..b-1]
        where [a, b] = sort [start, end]

expand isEmpty axisMap = do
    (i, c) <- zip [0..] axisMap
    if isEmpty V.! i then [c, c] else [c]

expandTest = expand (V.fromList [False, True, False, True]) "abcd"
-- >>> expandTest
-- "abbcd"

solve inputFilename = do
    charMap <- readCharMap inputFilename
    let nRows = V.length charMap
    let nCols = V.length $ V.head charMap
    let rows = [0..nRows-1]
    let cols = [0..nCols-1]
    let isEmptyRow = V.fromList $ do
        i <- rows
        return $ all (== '.') [charMap V.! i V.! j | j <- cols]
    let isEmptyCol = V.fromList $ do
        j <- cols
        return $ all (== '.') [charMap V.! i V.! j | i <- rows]

    let expandedMap = V.fromList . expand isEmptyRow . map (V.fromList . expand isEmptyCol . V.toList) . V.toList $ charMap
    let nRows = V.length expandedMap
    let nCols = V.length $ V.head expandedMap
    let rows = [0..nRows-1]
    let cols = [0..nCols-1]
    let galaxyCoordinates = [(i, j)|i <- rows, j <- cols, expandedMap V.! i V.! j == '#']
    let distance (a, b) (c, d) = abs (a - c) + abs (b - d)
    --      = let
    --         acDir = if a < c then 1 else -1
    --         bdDir = if b < d then 1 else -1
    --         correctionAB = minimum $ map dOfIsEmpty [isEmptyRow V.! (a + acDir), isEmptyCol V.! (b + bdDir)]
    --         correctionBD = minimum $ map dOfIsEmpty [isEmptyRow V.! (c - acDir), isEmptyCol V.! (d - bdDir)]
    --         delta = if a == c || b == d then 0 else correctionAB + correctionBD
    --      in distanceAxis isEmptyRow a c + distanceAxis isEmptyCol b d + delta
    let distances = map (uncurry distance) (pairs galaxyCoordinates)
    let solution1 = sum distances
    return $ solution1

-- >>> solve "inputs/sample/11.txt"
-- 374

-- >>> solve "inputs/real/11.txt"
-- 10494813
