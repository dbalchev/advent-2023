{-# LANGUAGE FlexibleContexts #-}
module Day23 where
import           AocParser               (readCharMap)
import qualified Data.Vector             as V

import           Data.Array.IO.Internals (IOArray (IOArray))
import qualified Data.Array.MArray       as MA
import Control.Monad (filterM, forM)

solve inputFilename = do
    hikeMap <- readCharMap inputFilename

    let
        nRows = V.length hikeMap
        nCols = V.length (V.head hikeMap)
        Just entryCol = V.findIndex (== '.') (V.head hikeMap)
        Just exitCol = V.findIndex (== '.') (V.last hikeMap)

        stepOptions (i, j) = case hikeMap V.! i V.! j of
            '.' -> [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]
            '^' -> [(i - 1, j)]
            '<' -> [(i, j - 1)]
            'v' -> [(i + 1, j)]
            '>' -> [(i, j + 1)]
        validStepOptions pos = [(i, j)| (i, j) <- stepOptions pos, 0 <= i && i < nRows && 0 <= j && j <= nCols && hikeMap V.! i V.! j /= '#']
    visited <-  MA.newArray ((0, 0), (nRows - 1, nCols - 1)) False :: IO (IOArray (Int, Int) Bool)

    let
        hike currentPath@(currentPos:_)
            | currentPos == (nRows - 1, exitCol) = return $ length currentPath
            | otherwise                          = do
                let currentPos = head currentPath
                MA.writeArray visited currentPos True
                unVisitedStepOptions <- filterM (fmap not <$> MA.readArray visited) (validStepOptions currentPos)
                pathLengths <- traverse (\nextPos -> hike (nextPos : currentPath)) unVisitedStepOptions
                MA.writeArray visited currentPos False
                return $ maximum (-1: pathLengths)
    maxPathLength <- hike [(0, entryCol)]

    let foo = 3
    return (maxPathLength - 1)

-- >>> solve "inputs/sample/23.txt"
-- 94

-- >>> solve "inputs/real/23.txt"
-- 2110
