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
        nextOf2 i j = do
            (ni, nj) <- [(i, j + 1), (i, j-1), (i + 1, j), (i-1, j)]
            let mi = ni `mod` nRows
            let mj = nj `mod` nCols
            guard $ (charMap V.! mi V.! mj) /= '#'
            return (mi, mj)
        allGardens =  do
            i <- [0..nRows - 1]
            j <- [0..nRows - 1]
            guard $ (charMap V.! i V.! j) /= '#'
            return (i, j)
        buildAdjMatrix :: IO (HT.BasicHashTable (Int, Int) (V.Vector (Int, Int)))
        buildAdjMatrix = do
            HT.fromList (map (\pos -> (pos, V.fromList . uncurry nextOf2 $ pos)) allGardens)
        buildIdentityMatrix = do
            HT.fromList (map (\pos -> (pos, V.singleton pos)) allGardens)
        multiplyVAM v b = do
            toVisit <- HT.new :: IO (HT.BasicHashTable (Int, Int) ())
            let
                insertFor pos = do
                    Just nexts <- HT.lookup b pos
                    forM_ nexts (\nn -> HT.insert toVisit nn ())
            forM_ v insertFor
            V.fromList . map fst <$> HT.toList toVisit
        multiplyAM a b = do
            result <- HT.new :: IO (HT.BasicHashTable (Int, Int) (V.Vector (Int, Int)))
            aList <- HT.toList a
            forM_ aList $ \(start, aNexts) -> do
                bNexts <- multiplyVAM aNexts b
                HT.insert result start bNexts
            return result
        
        powerM op a 0 acc = return acc
        powerM op a n acc = do
            let halfN = n `div` 2
            a2 <- op a a
            accAdj <- if n `mod` 2 == 1 then op acc a else return acc
            -- newAcc <- op a2 accAdj
            powerM op a2 halfN accAdj
        
    -- solution1 <- V.length <$> waveN nSteps (V.singleton (startI, startJ))
    adjMatrix <- buildAdjMatrix
    identityMatrix <- buildIdentityMatrix
    ampn <- powerM multiplyAM adjMatrix nSteps identityMatrix
    Just nn <- HT.lookup ampn (startI, startJ)
    
    return (V.length nn)

-- >>> solve 6 "inputs/sample/21.txt"
-- 16

-- >>> solve 50 "inputs/sample/21.txt"
-- 81

-- >>> solve 6 "inputs/sample/21.txt"


-- >>> solve 64 "inputs/real/21.txt"
-- ProgressCancelledException
