{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import           Control.Monad             (filterM, forM_, forever, when)
import           Control.Monad.ST          (runST)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Cont  (ContT (runContT), callCC)
import           Control.Monad.Trans.State (StateT (runStateT), get, put)
import qualified Data.HashTable.Class      as HTC
import qualified Data.HashTable.ST.Basic   as HT
import           Data.Maybe                (isNothing, isJust)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Vector               as V
import           GHC.ST                    (ST (ST))
import AocParser (readCharMap)


connections :: V.Vector (V.Vector Char) -> Int -> Int -> [(Int, Int)]
connections rawInput i j = metaConnection rawInput i j  $ rawInput V.! i V.! j

metaConnection :: V.Vector (V.Vector Char) -> Int -> Int -> Char -> [(Int, Int)]
metaConnection rawInput i j pipe
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
            north = [(i - 1, j) | i - 1 >= 0 && elem (rawInput V.! (i - 1) V.! j) ("|F7S" :: [Char])]
            south = [(i + 1, j) | i + 1 < nRows && elem (rawInput V.! (i + 1) V.! j) ("|JLS":: [Char])]
            west = [(i, j - 1) | j - 1 >= 0 && elem (rawInput V.! i V.! (j - 1)) ("-LFS":: [Char])]
            east = [(i, j + 1) | j + 1 < nCols && elem (rawInput V.! i V.! (j + 1)) ("-J7S":: [Char])]


isInside insideString trueSType = T.length (r remapped) `mod` 2 == 1
    where
        insideText = T.pack insideString
        remapped = T.replace "S" (T.pack [trueSType]) insideText
        go s = foldl (flip $ uncurry T.replace) (T.filter (/= '-') s) replacements
        replacements = [
                         ("L7", "|"),
                         ("LJ", ""),
                         ("F7", ""),
                         ("FJ", "|")
                        ]
        r s
            | go s == s && T.all (== '|') s = s
            | go s /= s = r (go s)


-- >>> isInside "S------7" 'F'
-- False

--- >>> isInside "|L-7" 'F'
-- False

--- >>> isInside "L-7" 'F'
-- True

--- >>> isInside "LJ" 'F'
-- False

solve inputFile = do
    rawInput <- readCharMap inputFile
    let nRows = V.length rawInput
    let nCols = V.length $ V.head rawInput
    let result = runST $ do
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
                lift . lift $ forM_  nextStates (flip (HT.insert visitedStep) (currentStep + 1))
                put (nextStates ++ stack, rest)
            return ()
        let [trueSType] = fst <$> filter ((== connections rawInput startI startJ) . snd) [(l, metaConnection rawInput startI startJ l)| l <- "LJ7F|-"]
        maxStep <- maximum . (snd <$>) <$> HTC.toList visitedStep
        let insideString i j = (fst <$>) <$> filterM ((isJust <$>) .  HT.lookup visitedStep . snd)  [(rawInput V.! i V.! k, (i, k)) |k <- [0..j - 1]]
        let isInsideIndex i j = do
            step <- HT.lookup visitedStep (i, j)
            str <- insideString i j
            return $ maybe (isInside str trueSType) (const False) step
        nInsideSquares <- length <$> filterM (uncurry isInsideIndex) [(i, j)| i <- [0..nRows -1], j <- [0..nCols - 1]]
        return (maxStep, trueSType, nInsideSquares)
    return result

-- >>> solve "inputs/sample/10.1.txt"
-- (8,'F',1)

-- >>> solve "inputs/sample/10.2.txt"
-- (23,'F',4)

-- >>> solve "inputs/sample/10.3.txt"
-- (22,'F',4)

-- >>> solve "inputs/sample/10.4.txt"
-- (70,'F',8)

-- >>> solve "inputs/sample/10.5.txt"
-- (80,'7',10)


-- >>> solve "inputs/real/10.txt"
-- (6717,'|',381)
