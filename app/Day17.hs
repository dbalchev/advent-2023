{-# LANGUAGE PartialTypeSignatures #-}
module Day17 where
import           AocParser                   (readCharMap)
import           Control.Monad               (when, (>=>), guard, forever, forM_)
import           Data.List                   (singleton)

import           Control.Monad.Trans.Except  (runExceptT, throwE)
import           Data.Function               (fix)
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as H
import qualified Data.Vector.Mutable         as MV
import qualified Data.HashTable.IO as HT
import Control.Monad.Trans.State (StateT(runStateT), get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Stack (HasCallStack)
import Data.Maybe (isNothing)

solve1 costMap = do
    let nRows = V.length costMap
    let nCols = V.length (V.head costMap)
    let llprint x = return () -- lift . lift . print $ x
    let makeState currentCost i j direction repeats = (currentCost + (nRows - i - 1) + nCols - j - 1, i, j, currentCost, direction, repeats::Int)
    initPQ <- MV.replicate 4 (makeState 0 0 0 (0, 0) 0)
    bestCost <- HT.new :: IO (HT.BasicHashTable _ Int )
    HT.insert bestCost (0, 0, (0, 0), 0) 0
    Left result <- runExceptT . (`runStateT` (initPQ, 1)) $ forever $ do
        (oldPQ, oldHeapSize) <- get
        llprint ("starting", oldHeapSize, MV.length oldPQ)
        (aStar, i, j, currentCost, lastDirection@(oldDI, oldDJ), repeats) <- MV.read oldPQ 0
        llprint ("ij", i, j, currentCost, aStar)
        when (oldHeapSize > 1) $ H.pop (flip compare) oldPQ 0 (oldHeapSize - 1)
        llprint "popping"
        put (oldPQ, oldHeapSize - 1)
        when (oldHeapSize - 1 + 4 > MV.length oldPQ) $ do
            llprint $ "resizing"
            (oldPQ, hs) <- get
            newPQ <- MV.grow oldPQ (MV.length oldPQ)
            put (newPQ, hs)
        Just currentBestCost <- lift . lift $ HT.lookup bestCost (i, j, lastDirection, repeats)
        when (currentBestCost >= currentCost) $ do
            forM_ [(0, 1), (1, 0), (-1, 0), (0, -1)] $ \newDirection@(di, dj) -> do
                let newI = i + di
                let newJ = j + dj
                let newCost = currentCost + (costMap V.! newI V.! newJ)
                let newRepeats = if newDirection == lastDirection then repeats + 1 else 1
                when (0 <= newI && newI < nRows && 0 <= newJ && newJ < nCols && newRepeats < 4 && (di /= -oldDI || dj /= -oldDJ)) $ do
                    when (newI == nRows - 1 && newJ == nCols - 1) $ do
                        lift $ throwE newCost
                    (pq, hs) <- get
                    let 
                        mutation Nothing = (Just newCost, True)
                        mutation (Just oldCost) = if oldCost > newCost then (Just newCost, True) else (Just oldCost, False)
                    shouldInsert <- lift . lift $ HT.mutate bestCost (newI, newJ, newDirection, newRepeats) mutation
                    when shouldInsert $ do
                        H.heapInsert (flip compare) pq 0 hs (makeState newCost newI newJ newDirection newRepeats)
                        llprint ("pushing", i, j, hs)
                        put (pq, hs + 1)
    return result

solve inputFilename = do
    costMap <- (fmap ((read :: String -> Int) . singleton) <$>) <$> readCharMap inputFilename
    solution1 <- solve1 costMap
    let foo = 3
    return solution1

-- >>> solve "inputs/sample/17.txt"
-- 102

-- >>> solve "inputs/real/17.txt"
-- 861
