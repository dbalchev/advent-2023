{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Day23 where
import           AocParser               (readCharMap)
import qualified Data.Vector             as V

import           Control.Monad           (filterM, forM, foldM, forM_)
import           Data.Array.IO.Internals (IOArray (IOArray))
import qualified Data.Array.MArray       as MA
import           Data.Bool               (bool)
import Data.Maybe (fromMaybe)

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
        extendPath tt initialPath = do
            let
                findExtendFrom i currentPath = do
                    currentVisited <- MA.newArray ((0, 0), (nRows - 1, nCols - 1)) 0 :: IO (IOArray (Int, Int) Int)
                    forM_ currentPath $ \pos -> MA.writeArray currentVisited pos 1
                    MA.writeArray currentVisited (currentPath V.! i) 2
                    let
                        go extensionPath@(currentPos:_) wasOutside = do
                            status <- MA.readArray currentVisited currentPos
                            if status == 1
                                then return (length extensionPath, seq (length extensionPath) extensionPath)
                                else do
                                    MA.writeArray currentVisited currentPos 3
                                    let
                                        foldfn (oldL, oldPath) nextPos = do
                                            (newL, newPath) <- go (nextPos: extensionPath) (wasOutside || status == 0)
                                            if oldL < newL
                                                then return (newL, newPath)
                                                else return (oldL, oldPath)
                                        maxStatus = bool 1 2 wasOutside
                                    unVisitedStepOptions <- filterM (fmap (< maxStatus) <$> MA.readArray currentVisited) (validStepOptions tt currentPos)
                                    pathToExtension <- foldM foldfn (0, []) unVisitedStepOptions
                                    MA.writeArray currentVisited currentPos 0
                                    return pathToExtension
                    go [currentPath V.! i] False
                extendFrom startIndex currentPath = do
                    (_, extension) <- findExtendFrom startIndex currentPath
                    -- print ("ext", extension)
                    if null extension
                        then return Nothing
                        else do
                            let
                                endPos = head extension
                                vExtension = V.fromList extension
                                Just endIndex = V.findIndex (==endPos) currentPath
                                (firstIndex, lastIndex) = if startIndex < endIndex then (startIndex, endIndex) else (endIndex, startIndex)
                                extensionToAdd = if startIndex < endIndex then V.reverse vExtension else vExtension
                                amendedPath = V.take (firstIndex - 1) currentPath V.++ extensionToAdd V.++ V.drop lastIndex currentPath
                                lengthGain = lastIndex - firstIndex < length extension
                            if startIndex < endIndex && lastIndex - firstIndex + 1 < length extension
                                then return $ Just amendedPath
                                else return Nothing
                go i p
                    | i == V.length p = return p
                    | otherwise = do
                        extended <- extendFrom i p
                        -- print extended
                        case extended of
                            Nothing -> go (i + 1) p
                            Just e -> go i e
            go 1 initialPath
        findMaxPath tt = do
            initialPath <- findAPath tt [(0, entryCol)]
            extendPath tt $ V.reverse (V.fromList initialPath)

    maxPath1 <- findMaxPath id
    maxPath2 <- findMaxPath (\c -> bool '#' '.' (c /= '#'))

    let foo = 3
    return (V.length maxPath1 - 1, V.length maxPath2 - 1)

-- >>> solve "inputs/sample/23.txt"
-- (90,154)

-- >>> solve "inputs/real/23.txt"
-- (2110,-1)
