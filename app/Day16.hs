{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day16 where
import           AocParser               (readCharMap)
import           Control.Monad           (filterM, forM_)
import qualified Data.Array.IO           as A
import qualified Data.Array.IO.Internals as A
import qualified Data.Array.MArray       as MA
import           Data.Bool               (bool)
import qualified Data.Vector             as V



nextBeams '.'  (di, dj) (i, j) = [((di, dj), (i + di, j + dj))]
nextBeams '-'  (0, dj) (i, j)  = [((0, dj), (i, j + dj))]
nextBeams '-'  (_, 0)  (i, j)  = [((0, 1), (i, j)), ((0, -1), (i, j))]
nextBeams '|'  (di, 0) (i, j)  = [((di, 0), (i + di, j))]
nextBeams '|'  (0, _)  (i, j)  = [((1, 0), (i, j)), ((-1, 0), (i, j))]
nextBeams '/'  (0, dj) (i, j)  = [((-dj, 0), (i - dj, j))]
nextBeams '/'  (di, 0)  (i, j) = [((0, -di), (i, j - di))]
nextBeams '\\' (0, dj) (i, j)  = [((dj, 0), (i + dj, j))]
nextBeams '\\' (di, 0) (i, j)  = [((0, di), (i, j + di))]

-- nextBeams currentChar (dx, dy) (i, j) = undefined
directionIndex (1, 0)  = 0
directionIndex (-1, 0) = 1
directionIndex (0, 1)  = 2
directionIndex (0, -1) = 3

solve inputFilename = do
    charMap <- readCharMap inputFilename
    let computeEnergized firstConfig = do
        visited :: (A.IOUArray (Int, Int, Int) Bool) <- MA.newArray ((0, 0, 0), (V.length charMap - 1, V.length (V.head charMap) -1, 4)) False
        let markVisited direction (i, j) = MA.writeArray visited (i, j, directionIndex direction) True
        let isVisited direction (i, j) = MA.readArray visited (i, j, directionIndex direction)
        let isValidIndex _ (i, j) = 0 <= i && i < V.length charMap && 0 <= j && j < V.length (V.head charMap)
        uncurry markVisited firstConfig
        let step direction position@(i, j) = do
            let next = filter (uncurry isValidIndex) $ nextBeams (charMap V.! i V.! j) direction position
            unvisitedNexts <- filterM (fmap not . uncurry isVisited) next
            forM_ unvisitedNexts (uncurry markVisited)
            forM_ unvisitedNexts (uncurry step)
        uncurry step firstConfig
        energised <- sequence $ do
            i <- [0..V.length charMap - 1]
            j <- [0..V.length (V.head charMap) - 1]
            let anyVisited = do
                k <- [0..3]
                return (MA.readArray visited (i, j, k) :: IO Bool)
            return (bool 0 1 . or <$> sequence anyVisited)
        return $ sum energised

    solution1 <- computeEnergized ((0, 1), (0, 0))
    let possibleStartsHorizontal = do
        row <- [0..V.length charMap - 1]
        [((0, 1), (row, 0)), ((0, -1), (row, V.length (V.head charMap) - 1))]
    let possibleStartsVertical = do
        col <- [0..V.length (V.head charMap) - 1]
        [((1, 0), (0, col)), ((-1, 0), (V.length charMap - 1, col))]
    solution2 <- maximum <$> traverse computeEnergized (possibleStartsHorizontal ++ possibleStartsVertical)
    return (solution1, solution2)

-- >>> solve "inputs/sample/16.txt"
-- (46,51)

-- >>> solve "inputs/real/16.txt"
-- (7482,7896)
