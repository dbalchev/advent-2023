{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
module Day20 where

import           Control.Monad       (forM_)
import           Data.Bool           (bool)
import qualified Data.HashTable.IO   as HT
import           Data.Maybe          (fromJust, fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

data ModuleType ff cc = Broadcaster | FlipFlop ff | Conjunction cc
    deriving (Show, Eq, Ord)

parseModule line = (moduleType, name, outputs)
    where
        [typeAndName, outputsString] = T.splitOn " -> " line
        outputs = V.fromList $ T.splitOn ", " outputsString
        firstTN = T.head typeAndName
        (moduleType, name)
            | firstTN == '%' = (FlipFlop (), T.tail typeAndName)
            | firstTN == '&' = (Conjunction (), T.tail typeAndName)
            | otherwise      = (Broadcaster, typeAndName)

-- >>> parseModule "broadcaster -> a, b, c"
-- (Broadcaster,"broadcaster",["a","b","c"])

-- >>> parseModule "%a -> b"
-- (FlipFlop,"a",["b"])

data CompiledModule = CompiledModule (ModuleType () (V.Vector Int)) T.Text Int (V.Vector CompiledModule)

updatePulses 0 [a, b] = [a + 1, b]
updatePulses 1 [a, b] = [a, b + 1]

processPulse moduleStates pulse _ (CompiledModule Broadcaster name moduleIndex outputs) = do
    let nextActions = map (pulse,moduleIndex, ) $ V.toList outputs
    return nextActions
processPulse moduleStates 1 _ (CompiledModule (FlipFlop ()) name moduleIndex outputs) = do
    return []
processPulse moduleStates 0 _ (CompiledModule (FlipFlop ()) name moduleIndex outputs) = do
    FlipFlop oldState <- MV.read moduleStates moduleIndex
    let newState = 1 - oldState
    MV.write moduleStates moduleIndex (FlipFlop newState)
    let nextActions = map (newState,moduleIndex,) $ V.toList outputs
    return nextActions
processPulse moduleStates pulse inputModuleIndex (CompiledModule (Conjunction inputRemap) name moduleIndex outputs) = do
    Conjunction state <- MV.read moduleStates moduleIndex
    let inputIndex = inputRemap V.! inputModuleIndex
    MV.write state inputIndex pulse
    allHigh <- MV.foldl (\acc c -> acc && (c == 1)) True state
    let nextPulse = bool 1 0 allHigh
    let nextActions = map (nextPulse,moduleIndex,) $ V.toList outputs
    return nextActions





makeStep moduleStates ([], []) = do
    return [0, 0]

makeStep moduleStates (stack, []) = makeStep moduleStates ([], reverse stack)
makeStep moduleStates (oldStack, (pulse, inputModuleIndex, currentModule): restQueue) = do
    nextActions <- processPulse moduleStates pulse inputModuleIndex currentModule
    nextResults <- makeStep moduleStates (nextActions ++ oldStack, restQueue)
    return $ updatePulses pulse nextResults

solve inputFilename = do
    parsedModules <- V.fromList . fmap parseModule . T.lines <$> T.readFile inputFilename

    (broadcaster, allModules) <- mdo
        nameToIndex <- HT.new :: IO (HT.BasicHashTable a b)
        nameToInputNames <- HT.new :: IO (HT.BasicHashTable a b)

        forM_ (zip [0..] $ V.toList parsedModules) $ \(i, (moduleType, name, outputs)) -> do
            HT.insert nameToIndex name i
            forM_ outputs $ \output -> do
                HT.mutate nameToInputNames output (\oldState -> (Just (name: fromMaybe [] oldState), ()))

        let
            indexFromName name = fromJust <$> HT.lookup nameToIndex name
            fromName name = (allModules V.!) <$> indexFromName name
            compileModule (moduleType, name, outputs) = do
                print ("fixing", name)
                Just moduleIndex <- HT.lookup nameToIndex name
                compiledOutputs <- traverse fromName outputs
                let
                    compileModuleType Broadcaster   = return Broadcaster
                    compileModuleType (FlipFlop ()) = return $ FlipFlop ()
                    compileModuleType (Conjunction ()) = do
                        Just inputs <- HT.lookup nameToInputNames name
                        inputIndices <- traverse indexFromName inputs
                        let result = V.replicate (V.length parsedModules) (-1) V.// zip inputIndices [0..]
                        return $ Conjunction result
                compiledModuleType <- compileModuleType moduleType
                return $ CompiledModule compiledModuleType name moduleIndex compiledOutputs
        allModules <- traverse compileModule parsedModules
        broadcaster <- fromName "broadcaster"

        return (broadcaster, allModules)


    let foo = 3
    return broadcaster

-- >>> solve "inputs/sample/20.1.txt"
-- [(Broadcaster,"broadcaster",["a","b","c"]),(FlipFlop,"a",["b"]),(FlipFlop,"b",["c"]),(FlipFlop,"c",["inv"]),(Conjunction,"inv",["a"])]
