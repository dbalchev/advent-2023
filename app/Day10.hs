module Day10 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.HashTable.Class as HTC
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST (runST)
import Data.Maybe (isNothing)
import Control.Monad (filterM, when, forM_, forever)
import GHC.ST (ST(ST))
import Control.Monad.Trans.State (get, StateT (runStateT), put)
import Control.Monad.Trans.Cont (ContT(runContT), callCC)
import Control.Monad.Trans.Class (MonadTrans(lift))


connections :: V.Vector (V.Vector Char) -> Int -> Int -> [(Int, Int)]
connections rawInput i j
    | pipe == '|' = north ++ south
    | pipe == '-' = west ++ east
    | pipe == 'L' = north ++ east
    | pipe == 'J' = north ++ west
    | pipe == '7' = south ++ west
    | pipe == 'F' = south ++ east
    | pipe == 'S' = north ++ south ++ west ++ east
    | otherwise = []
        where
            nRows = V.length rawInput
            nCols = V.length $ V.head rawInput
            pipe = rawInput V.! i V.! j
            north = [(i - 1, j) | i - 1 >= 0 && elem (rawInput V.! (i - 1) V.! j) "|F7S"]
            south = [(i + 1, j) | i + 1 < nRows && elem (rawInput V.! (i + 1) V.! j) "|JLS"]
            west = [(i, j - 1) | j - 1 >= 0 && elem (rawInput V.! i V.! (j - 1)) "-LFS"]
            east = [(i, j + 1) | j + 1 < nCols && elem (rawInput V.! i V.! (j + 1)) "-J7S"]



solve inputFile = do
    rawInput <- V.fromList . (V.fromList . T.unpack <$>) . T.lines <$> T.readFile inputFile
    let nRows = V.length rawInput
    let nCols = V.length $ V.head rawInput
    let solution1 = runST $ do
        visitedStep <- HT.new
        let [(startI, startJ)] = [(i, j)| i <- [0..nRows -1], j <- [0..nCols - 1], rawInput V.! i V.! j == 'S']
        HT.insert visitedStep (startI, startJ) 0
        let explore (i, j) = do
            let nextSteps = connections rawInput i j
            filterM ((isNothing <$>) <$> HT.lookup visitedStep) nextSteps
        (`runContT` return) $ callCC $ \break -> do
            (`runStateT` ([], [(startI, startJ)])) $ forever $ do
                (stack, queue) <- get
                when (null stack && null queue) $ lift $ break ()
                when (null queue) $ put ([], reverse stack)
                (stack, current:rest) <- get
                Just currentStep <- lift . lift $ HT.lookup visitedStep current
                nextStates <- lift . lift $ explore current
                lift . lift $ forM_  nextStates (flip (HT.insert visitedStep)(currentStep + 1))
                put (nextStates ++ stack, rest)
            return ()
        maxStep <- maximum . (snd <$>) <$> HTC.toList visitedStep 
        return (maxStep)
    return (solution1)

-- >>> solve "inputs/sample/10.txt"
-- 8

-- >>> solve "inputs/real/10.txt"
-- 6717
