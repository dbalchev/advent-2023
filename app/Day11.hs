{-# LANGUAGE TupleSections #-}
module Day11 where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V
import AocParser (readCharMap)
import Control.Monad (guard)
import Data.List (tails, sort)
import Utils (pairs)

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

solveOnce expansionFactor charMap = sum distances
    where
        nRows = V.length charMap
        nCols = V.length $ V.head charMap
        rows = [0..nRows-1]
        cols = [0..nCols-1]
        isEmptyRow = V.fromList $ do
            i <- rows
            return $ all (== '.') [charMap V.! i V.! j | j <- cols]
        isEmptyCol = V.fromList $ do
            j <- cols
            return $ all (== '.') [charMap V.! i V.! j | i <- rows]
        emptyRowsUntil = V.scanl1 (+) $ boolToInt <$> isEmptyRow
        emptyColsUntil = V.scanl1 (+) $ boolToInt <$> isEmptyCol
        galaxyCoordinates = [
                                (i + expansionFactor * (emptyRowsUntil V.! i), j +  expansionFactor * (emptyColsUntil V.! j))
                                |i <- rows, j <- cols, charMap V.! i V.! j == '#'
                            ]
        distance (a, b) (c, d) = abs (a - c) + abs (b - d)
        distances = map (uncurry distance) (pairs galaxyCoordinates)

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

    let solution1 = solveOnce 1 charMap
    let solution2 = solveOnce (1000000 - 1) charMap
    return (solution1, solution2)

-- >>> solve "inputs/sample/11.txt"
-- (374,82000210)

-- >>> solve "inputs/real/11.txt"
-- (10494813,840988812853)
