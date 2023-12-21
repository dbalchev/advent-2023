{-# LANGUAGE TupleSections #-}
module Day21 where
import           AocParser         (readCharMap)

import           Control.Monad     (guard, forM_)
import qualified Data.HashTable.IO as HT
import qualified Data.Vector       as V



solve nSteps inputFilename = do
    charMap <- readCharMap inputFilename

    let
        nRows = V.length charMap
        nCols = V.length (V.head charMap)
        [(startI, startJ)] = do
            i <- [0..nRows - 1]
            j <- [0..nRows - 1]
            guard $ (charMap V.! i V.! j) == 'S'
            return (i, j)
        nextOf i j = do
            (ni, nj) <- [(i, j + 1), (i, j-1), (i + 1, j), (i-1, j)]
            guard $ (0 <= ni) && (ni < nRows) && (0 <= nj) && (nj < nCols)
            guard $ (charMap V.! ni V.! nj) /= '#'
            return (ni, nj)
        nextWave wave = do
            let nextWithDuplicates = concatMap (uncurry nextOf) (V.toList wave)
            noDuplicatesHT <- HT.fromList (map (,()) nextWithDuplicates) :: IO (HT.BasicHashTable (Int, Int) ())
            V.fromList . map fst <$> HT.toList noDuplicatesHT
        waveN 0 wave = return wave
        waveN n wave = nextWave wave >>= waveN (n - 1)
    solution1 <- V.length <$> waveN nSteps (V.singleton (startI, startJ))
    let foo = 3
    return solution1

-- >>> solve 6 "inputs/sample/21.txt"
-- 16

-- >>> solve 64 "inputs/real/21.txt"
-- 3776
