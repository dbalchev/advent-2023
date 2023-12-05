{-# LANGUAGE FlexibleContexts #-}
module Day05 where

import           AocParser           (intParser)
import           Control.Applicative (asum)
import           Control.Exception   (Exception (toException), throw)
import           Data.List           (group, sort, uncons)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as DV
import           GHC.IO              (unsafePerformIO)
import           Text.Parsec         (letter, many, newline, sepBy, spaces,
                                      string)
import           Text.Parsec.String  (Parser, parseFromFile)
type Map = (T.Text, T.Text, Vector (Int, Int, Int))
type Almanac = (Vector Int, Vector Map)

mapParser :: Parser Map
mapParser = do
    fromName <- T.pack <$> many letter
    string "-to-"
    toName <-  T.pack <$> many letter
    string " map:"
    newline
    triplets <- many $ do
        destStart <- intParser
        spaces
        sourceStart <- intParser
        spaces
        rangeLen <- intParser
        return (destStart, sourceStart, rangeLen)
    return (fromName, toName, DV.fromList triplets)


almanacParser = do
    string "seeds: "
    seeds <- DV.fromList <$> sepBy intParser spaces
    map <- DV.fromList <$> many mapParser
    return (seeds, map)

fst3 (a, _, _) = a
snd3 (_, a, _) = a
trd3 (_, _, a) = a

rangeMatch seed (destStart, sourceStart, rangeLen)
    = if offset >= 0 && offset < rangeLen then Just (destStart + offset) else Nothing
        where
            offset = seed - sourceStart

mapOnce seed = maybe seed id . asum . map (rangeMatch seed) . DV.toList

fullyMapSeed :: Int -> Vector (Vector (Int, Int, Int)) -> Int
fullyMapSeed = DV.foldl mapOnce

filterEmptyRange = filter ((/= 0) . snd)
filterEmptyRanges (a, b) = (filterEmptyRange a, filterEmptyRange b)

mapRangeOnce (seedStart, seedLen) (destStart, sourceStart, rangeLen)
    --  | --- seed -- | | -- source -- |
    | seedEnd <= sourceStart  = filterEmptyRanges ([(seedStart, seedLen)], [])
    --  | -- seed -- |
    --           | -- source -- |
    | seedStart <= sourceStart && sourceStart <= seedEnd &&  seedEnd <= sourceEnd  = filterEmptyRanges ([(seedStart, sourceStart - seedStart)], [(destStart, seedEnd - sourceStart)])
    --  | -- seed  --        |
    --     | -- source -- |
    | seedStart <= sourceStart && seedEnd >= sourceEnd = filterEmptyRanges ([(seedStart, sourceStart - seedStart), (sourceEnd, seedEnd - sourceEnd)], [(destStart, rangeLen)])
    --     | -- seed -- |
    --  | -- source  --    |
    | sourceStart <= seedStart && seedEnd <= sourceEnd = filterEmptyRanges ([], [(destStart + seedStart - sourceStart, seedLen)])
    --           | -- seed -- |
    --  | -- source -- |
    | sourceStart <= seedStart && sourceEnd <= seedEnd && seedStart <= sourceEnd = filterEmptyRanges ([(sourceEnd, seedEnd - sourceEnd)], [(destStart + seedStart - sourceStart, sourceEnd - seedStart)])
        --  | --- source -- | | -- seed -- |
    | sourceEnd <= seedStart = filterEmptyRanges ([(seedStart, seedLen)], [])
    | otherwise = unsafePerformIO $ fail $ show ((seedStart, seedLen), (destStart, sourceStart, rangeLen))
    --
        where
            seedEnd = seedStart + seedLen
            sourceEnd = sourceStart + rangeLen

mapRange seedRanges = go (seedRanges, [])
    where
        go (unmapped, mapped) []             = map head . group . sort $ unmapped ++ mapped
        go (oldUnmapped, oldMapped) (x : xs) = go (newUnmapped, oldMapped ++ newMapped) xs
            where
                newList = map (`mapRangeOnce` x) oldUnmapped
                newUnmapped = concatMap fst newList
                newMapped   = concatMap snd newList

makePairs []             = []
makePairs (a : b : rest) = (a, b) : makePairs rest

solve inputFilename = do
    Right parsedInput@(seeds, maps) <- parseFromFile almanacParser inputFilename
    let inputTest = and $ zipWith (==) (map snd3 $ DV.toList maps) (map fst3 $ tail $ DV.toList maps)
    let testedMaps = DV.map trd3 maps
    let mappedSeeds = DV.map (`fullyMapSeed` testedMaps) seeds
    let solution1 = DV.minimum mappedSeeds
    let seedRanges = DV.fromList . makePairs .DV.toList $ seeds
    let solution2 = minimum . map fst $ DV.foldl mapRange (DV.toList seedRanges) (DV.toList <$> testedMaps)
    return (inputTest, solution1, solution2)

-- >>> solve "inputs/sample/05.txt"
-- (True,35,46)

-- >>> solve "inputs/real/05.txt"
-- (True,199602917,2254686)
