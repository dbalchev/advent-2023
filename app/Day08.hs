{-# LANGUAGE OverloadedStrings #-}
module Day08 where

import           Control.Monad           (foldM, forM_, when)
import           Control.Monad.ST        (runST)
import qualified Data.HashTable.ST.Basic as HT
import           Data.Maybe              (fromJust, isJust)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as VM
import           Text.Parsec             (digit, letter, many, parse, string,
                                          (<|>))
import           Text.Parsec.Text        (Parser)
import Control.Monad.Trans.Cont (callCC, ContT (runContT))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))

lineParser :: Parser (T.Text, (T.Text, T.Text))
lineParser = do
    source <- T.pack <$> many (letter <|> digit)
    string " = ("
    leftDestination <- T.pack <$> many (letter <|> digit)
    string ", "
    rightDesination <- T.pack <$> many (letter <|> digit)
    string ")"
    return (source, (leftDestination, rightDesination))

-- >>> parse lineParser "" $ T.pack "BBB = (DDD, EEE)"
-- Right ("BBB",("DDD","EEE"))

seek 'L' = fst
seek 'R' = snd

findCycles transitionMap instructions isFinal startingState  = do
    instructionXstateToStep <- HT.new
    let step i currentState finalStates = do
        let instructionIndex = i `mod` T.length instructions
        let key = (instructionIndex, currentState)
        let unfinishedStep = do
            HT.insert instructionXstateToStep key i
            nextState <- (seek . T.index instructions $  instructionIndex) . fromJust <$> HT.lookup transitionMap currentState
            let currentFinalStates = ([i | isFinal currentState])
            step (i + 1) nextState (finalStates ++ currentFinalStates)
        let finishedStep prevIndex = return (prevIndex, i, finalStates)
        lookupResult <- HT.lookup instructionXstateToStep key
        maybe unfinishedStep finishedStep lookupResult
    step 0 startingState []

endsWith c = (== c) . snd . fromJust . T.unsnoc

computeChineseTheoremRequirements cycleInfo = do
    (cycleStartStep, cycleEndStep, finalStates) <- cycleInfo
    let cycleLen = cycleEndStep - cycleStartStep
    let requirements = [(cycleLen, finalState)|finalState <- finalStates]
    return requirements

solve inputFilename = do
    let limit = 50000000
    inputLines <- T.lines <$> T.readFile inputFilename
    let instructions = head inputLines
    let Right parsedInput = traverse (parse lineParser "") $ drop 2 inputLines
    let result = runST $ do
        ht <- HT.new
        forM_ parsedInput (uncurry (HT.insert ht))
        let repInstructions = cycle $ T.unpack instructions
        let nextState i = ((seek i . fromJust) <$>) <$> HT.lookup ht
        let nextStates = map nextState repInstructions
        hasAAA <- isJust <$> HT.lookup ht "AAA"
        hasZZZ <- isJust <$> HT.lookup ht "ZZZ"
        let walk = (`runContT` id) $ callCC $ \exit -> do
            (`runStateT` (0 :: Int, "AAA")) $ do
                forM_ nextStates $ \next -> do
                    (oldCount, oldState) <- get
                    when (oldState == "ZZZ") (lift $ exit $ return oldCount)
                    newState <- lift . lift $ next oldState
                    put (oldCount + 1, newState)
            return $ return $ -1
        solution1 <- if hasAAA && hasZZZ then walk else return $ -1
        let beginningStates = V.fromList $ filter (endsWith 'A') . map fst $ parsedInput
        cycleInfo <- mapM (findCycles ht instructions (endsWith 'Z')) beginningStates
        let requirements = computeChineseTheoremRequirements cycleInfo
        let cycleLengths = toInteger . fst . head <$> requirements
        -- solution2 <- walk beginningStates (map mapM nextStates)  (all $ endsWith 'Z') limit
        let cycleLcm = foldl1 lcm (V.toList cycleLengths)
        return (solution1, requirements, cycleLcm)
    return result

-- >>> solve "inputs/sample/08.1.txt"
-- (2,[[(2,2),(2,3)]],2)

-- >>> solve "inputs/sample/08.2.txt"
-- (6,[[(3,6),(3,7),(3,8)]],3)

-- >>> solve "inputs/sample/08.3.txt"
-- (-1,[[(2,2)],[(6,3),(6,6)]],6)
-- >>> solve "inputs/real/08.txt"
-- (19783,[[(11653,11653)],[(19783,19783)],[(19241,19241)],[(16531,16531)],[(12737,12737)],[(14363,14363)]],9177460370549)
