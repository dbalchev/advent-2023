{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.List (transpose, uncons)
import Data.Either (isRight)
import Utils (vTranspose)

parseToVector = V.fromList . (V.fromList . T.unpack <$>) . T.lines

hasHorizontalMirrorAt terrainMap i = V.and $ V.zipWith (==) mirroredNorthPart southPart
    where
        (northPart, southPart) = V.splitAt i terrainMap
        mirroredNorthPart = V.reverse northPart

solve1 terrainMap =  rowScores ++ colScores
    where
        rows = [1..(V.length terrainMap - 1)]
        transposed = vTranspose terrainMap
        cols = [1..(V.length transposed - 1)]
        rowScores = ((100 *) <$>) . filter (hasHorizontalMirrorAt terrainMap) $ rows
        colScores = filter (hasHorizontalMirrorAt transposed) cols


fixPixel '.' = '#'
fixPixel '#' = '.'

fixTerrainMap terrainMap i j = terrainMap V.// [(i,rowToFix V.// [(j, fixPixel (rowToFix V.! j))])]
    where
        rowToFix = terrainMap V.! i

solve2 terrainMap = concatMap (filter (not . (`elem` originalSolutions)) . solve1 . uncurry (fixTerrainMap terrainMap)) pixels
    where
        originalSolutions = solve1 terrainMap
        pixels = (,) <$> [0..(V.length terrainMap - 1)] <*> [0..(V.length (V.head terrainMap) - 1)]


solve inputFilename = do
    input <- V.fromList . (parseToVector<$>) . T.splitOn "\n\n" <$> T.readFile inputFilename
    let solution1 = (sum <$>) . traverse (maybe (Left ()) (Right . fst) . uncons . solve1) $ V.toList input
    let solution2 = (sum <$>) . traverse (maybe (Left ()) (Right . fst) . uncons . solve2) $ V.toList input

    return (solution1, solution2)

-- >>> solve "inputs/sample/13.txt"
-- (Right 405,Right 400)

-- >>> solve "inputs/real/13.txt"
-- (Right 33975,Right 29083)

