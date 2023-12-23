{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Day23 where
import           AocParser               (readCharMap)
import qualified Data.Vector             as V

import           Control.Monad           (filterM, forM, foldM, forM_)
import           Data.Array.IO.Internals (IOArray (IOArray))
import qualified Data.Array.MArray       as MA
import           Data.Bool               (bool)

solve inputFilename = do
    hikeMap <- readCharMap inputFilename

    let
        nRows = V.length hikeMap
        nCols = V.length (V.head hikeMap)
        Just entryCol = V.findIndex (== '.') (V.head hikeMap)
        Just exitCol = V.findIndex (== '.') (V.last hikeMap)

        stepOptions tt (i, j) = case  tt (hikeMap V.! i V.! j) of
            '.' -> [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]
            '^' -> [(i - 1, j)]
            '<' -> [(i, j - 1)]
            'v' -> [(i + 1, j)]
            '>' -> [(i, j + 1)]
        validStepOptions tt pos = [(i, j)| (i, j) <- stepOptions tt pos, 0 <= i && i < nRows && 0 <= j && j <= nCols && hikeMap V.! i V.! j /= '#']
    visited <-  MA.newArray ((0, 0), (nRows - 1, nCols - 1)) False :: IO (IOArray (Int, Int) Bool)

    let
        hike tt currentPath@(currentPos:_)
            | currentPos == (nRows - 1, exitCol) = return $ length currentPath
            | otherwise                          = do
                let currentPos = head currentPath
                MA.writeArray visited currentPos True
                unVisitedStepOptions <- filterM (fmap not <$> MA.readArray visited) (validStepOptions tt currentPos)
                pathLengths <- traverse (\nextPos -> hike tt (nextPos : currentPath)) unVisitedStepOptions
                MA.writeArray visited currentPos False
                return $ maximum (-1: pathLengths)
        findAPath tt currentPath@(currentPos:_)
            | currentPos == (nRows - 1, exitCol) = return currentPath
            | otherwise                          = do
                let
                    currentPos = head currentPath
                    foldfn [] nextPos = findAPath tt (nextPos: currentPath)
                    foldfn path _ = return path
                MA.writeArray visited currentPos True
                unVisitedStepOptions <- filterM (fmap not <$> MA.readArray visited) (validStepOptions tt currentPos)
                pathToEnd <- foldM foldfn [] unVisitedStepOptions
                MA.writeArray visited currentPos False
                return pathToEnd
        extendPath tt currentPath = do
            currentVisited <- MA.newArray ((0, 0), (nRows - 1, nCols - 1)) 0 :: IO (IOArray (Int, Int) Int)
            forM_ currentPath $ \pos -> MA.writeArray currentVisited pos 1
            let
                findExtendFrom tt extensionPath@(currentPos:_) = do
                    status <- MA.readArray currentVisited currentPos
                    if status == 1
                        then return extensionPath
                        else do
                            MA.writeArray currentVisited currentPos 2
                            let
                                foldfn [] nextPos = findExtendFrom tt (nextPos: extensionPath)
                                foldfn path _ = return path
                            unVisitedStepOptions <- filterM (fmap (/= 1) <$> MA.readArray currentVisited) (validStepOptions tt currentPos)
                            pathToExtension <- foldM foldfn [] unVisitedStepOptions
                            MA.writeArray currentVisited currentPos 0
                            return pathToExtension
                extendFrom tt startingPos = do
                    extension <- findExtendFrom tt [startingPos]
                    if null extension
                        then return Nothing
                        else do
                            let
                                endPos = head extension
                                Just startIndex = V.findIndex (==startingPos) currentPath
                                Just endIndex = V.findIndex (==endPos) currentPath
                                (firstIndex, lastIndex) = if startIndex < endIndex then (startIndex, endIndex) else (endIndex, startIndex)
                                extensionToAdd = if startIndex < endIndex then reverse extension else extension
                                amendedPath = V.take firstIndex currentPath V.++ V.fromList extensionToAdd V.++ V.drop lastIndex currentPath
                            if lastIndex - firstIndex < length extension
                                then return $ Just amendedPath
                                else return Nothing
            return [3]

    maxPathLength1 <- hike id [(0, entryCol)]
    path2 <- findAPath (\c -> bool '#' '.' (c /= '#')) [(0, entryCol)]

    let foo = 3
    return (maxPathLength1 - 1, length path2)

-- >>> solve "inputs/sample/23.txt"
-- (94,83)

-- >>> solve "inputs/real/23.txt"
-- ProgressCancelledException
