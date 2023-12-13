{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.List (transpose, uncons)

parseToVector = V.fromList . (V.fromList . T.unpack <$>) . T.lines

hasHorizontalMirrorAt terrainMap i = V.and $ V.zipWith (==) mirroredNorthPart southPart
    where
        (northPart, southPart) = V.splitAt i terrainMap
        mirroredNorthPart = V.reverse northPart

vTranspose = V.fromList . (V.fromList <$>) . transpose . (V.toList <$>) . V.toList

solve1 terrainMap = do
    let rows = [1..(V.length terrainMap - 1)]
    maybe (Right ()) (Left . (100 *) . fst) $ uncons . filter (hasHorizontalMirrorAt terrainMap) $ rows
    let transposed = vTranspose terrainMap
    let cols = [1..(V.length transposed - 1)]
    maybe (Right ()) (Left . fst) $ uncons . filter (hasHorizontalMirrorAt transposed) $ cols
    return ()

solve inputFilename = do
    input <- V.fromList . (parseToVector<$>) . T.splitOn "\n\n" <$> T.readFile inputFilename
    let solution1 = (sum <$>) . traverse (either Right Left . solve1) $ V.toList input
    return solution1

-- >>> solve "inputs/sample/13.txt"
-- Right 405

-- >>> solve "inputs/real/13.txt"
-- Right 33975

