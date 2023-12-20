{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
module Day20 where

import           Control.Monad       (forM_)
import qualified Data.HashTable.IO   as HT
import           Data.Maybe          (fromJust)
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

processPulse moduleStates pulse inputIndex (CompiledModule Broadcaster name moduleIndex outputs) = do
    let nextActions = map (pulse,moduleIndex, ) $ V.toList outputs
    return (updatePulses pulse [0, 0], nextActions)
processPulse moduleStates 1 inputIndex (CompiledModule (FlipFlop ()) name moduleIndex outputs) = do
    return ([0, 1], [])
processPulse moduleStates 0 inputIndex (CompiledModule (FlipFlop ()) name moduleIndex outputs) = do
    Left oldState <- MV.read moduleStates moduleIndex
    let newState = 1 - oldState
    MV.write moduleStates moduleIndex (Left newState)
    let nextActions = map (newState,moduleIndex,) $ V.toList outputs
    return ([1, 0], nextActions)
-- processPulse moduleStates pulse inputIndex (CompiledModule Conjunction name moduleIndex outputs) = do
--     Right state <- MV.read moduleStates moduleIndex




makeStep moduleStates ([], []) = do
    return [0, 0]

makeStep moduleStates (stack, []) = makeStep moduleStates ([], reverse stack)
makeStep moduleStates (oldStack, (pulse, currentModule): restQueue) = return [0, 0]

solve inputFilename = do
    parsedModules <- V.fromList . fmap parseModule . T.lines <$> T.readFile inputFilename

    (broadcaster, allModules) <- mdo
        nameToIndex <- HT.new :: IO (HT.BasicHashTable a b)
        nameToInputNames <- HT.new :: IO (HT.BasicHashTable a b)

        forM_ (zip [0..] $ V.toList parsedModules) $ \(i, (moduleType, name, outputs)) -> do
            HT.insert nameToIndex name i
            forM_ outputs $ \output -> do
                HT.mutate nameToInputNames output (\oldState -> Just (name: fromMaybe [] oldState))

        let fromName name = (allModules V.!) . fromJust <$> HT.lookup nameToIndex name
        let
            compileModule (moduleType, name, outputs) = do
                print ("fixing", name)
                Just moduleIndex <- HT.lookup nameToIndex name
                compiledOutputs <- traverse fromName outputs
                return $ CompiledModule moduleType name moduleIndex compiledOutputs
        allModules <- traverse compileModule parsedModules
        broadcaster <- fromName "broadcaster"

        return (broadcaster, allModules)


    let foo = 3
    return broadcaster

-- >>> solve "inputs/sample/20.1.txt"
-- [(Broadcaster,"broadcaster",["a","b","c"]),(FlipFlop,"a",["b"]),(FlipFlop,"b",["c"]),(FlipFlop,"c",["inv"]),(Conjunction,"inv",["a"])]
