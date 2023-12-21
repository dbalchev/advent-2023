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
import Data.List (groupBy, group, sort)

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

extractModuleType (CompiledModule moduleType _ _ _ ) = moduleType

makeStep moduleStates ([], []) acc = do
    return acc

makeStep moduleStates (stack, []) acc = makeStep moduleStates ([], reverse stack) acc
makeStep moduleStates (oldStack, (pulse, inputModuleIndex, currentModule): restQueue) acc = do
    -- let (CompiledModule moduleType name _ _) = currentModule 
    -- print ("pulse", pulse, name, moduleType)
    nextActions <- processPulse moduleStates pulse inputModuleIndex currentModule
    makeStep moduleStates (nextActions ++ oldStack, restQueue) (updatePulses pulse acc)

makeModuleInitialState Broadcaster = return Broadcaster
makeModuleInitialState (FlipFlop ()) = return $ FlipFlop 0
makeModuleInitialState (Conjunction inputRemap) = do
    let stateSize = 1 + V.maximum inputRemap
    Conjunction <$> MV.replicate stateSize (-1)

makeInitialState ::  V.Vector (ModuleType () (V.Vector Int)) -> IO (V.Vector (ModuleType Int (MV.MVector (MV.PrimState IO) Int)))
makeInitialState = traverse makeModuleInitialState

solve inputFilename = do
    parsedModulesRaw <- V.fromList . fmap parseModule . T.lines <$> T.readFile inputFilename
    let parsedNames = map (\(_, n, _) -> n) $ V.toList parsedModulesRaw
    let outputNames = map head . group . sort . concatMap (\(_, _, o) -> V.toList o) $ parsedModulesRaw
    let parsedModules = parsedModulesRaw V.++ V.fromList [(Broadcaster, n, V.empty) | n <- filter (not . (`elem` parsedNames))outputNames]
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
    moduleStateV <- makeInitialState $ fmap extractModuleType allModules
    moduleState <- V.thaw moduleStateV
    let 
        repSteps :: Int -> [Int] -> IO [Int]
        repSteps 0 acc = return acc
        repSteps n acc = do
            newAcc <- makeStep moduleState ([], [(0, -1, broadcaster)]) acc
            repSteps (n-1) newAcc
    pulses <- repSteps 1000 [0, 0]
    return (pulses, product pulses)

-- >>> solve "inputs/sample/20.1.txt"
-- ([8000,4000],32000000)

-- >>> solve "inputs/sample/20.2.txt"
-- ([4250,2750],11687500)

-- >>> solve "inputs/real/20.txt"
-- ([17529,47116],825896364)
