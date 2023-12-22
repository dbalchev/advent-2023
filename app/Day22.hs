{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day22 where

import           Control.Monad           (forM_)
import           Data.Array.IO.Internals (IOArray (IOArray))
import qualified Data.Array.MArray       as MA
import           Data.List               (group, sort, sortBy, sortOn)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Vector             as V
readLine line = tokens
    where
        tokens :: [[Int]]
        tokens = fmap ((read . T.unpack <$>) . T.splitOn ",") . T.splitOn "~" $ line
        -- toPair [a, b] = (a,b)
        -- toTriple [a, b, c] = (a, b, c)
        -- points = toPair $ fmap toTriple tokens

getZ (_, _, z) = z

sortCriteria :: (Int, [[Int]]) -> Int
sortCriteria = minimum . map last . snd

-- >>> sortCriteria (5, [[1,1,8],[1,1,9]])
-- 8

process1 points = do
    let
        numPoints = zip [0..] points
        extract f = concatMap (map f) points
        minX = minimum (extract head)
        maxX = maximum (extract head)
        minY = minimum (extract (head . tail))
        maxY = maximum (extract (head . tail))

        sortedPoints = sortOn sortCriteria numPoints

    currentTop <- MA.newArray ((minX, minY), (maxX, maxY)) (0, -1) :: IO (IOArray (Int, Int) (Int, Int))
    let
        step [] acc = return acc
        step ((i, [[x1, y1, z1], [x2, y2, z2]]):nextPoints) acc = do
            let volumePoints = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
            topState <- traverse (MA.readArray currentTop) volumePoints
            let topZ = maximum . map fst $ topState
            let supportingIndices = map head . group . sort . filter (/= (-1)) . map snd . filter ((== topZ) . fst) $ topState
            let minZ = min z1 z2
            let maxZ = max z1 z2
            let newTopZ = maxZ - (minZ - topZ - 1)
            forM_ volumePoints $ \point -> do
                MA.writeArray currentTop point (newTopZ, i)
            -- print ("location", i, newTopZ, supportingIndices, topState, topZ)
            step nextPoints $ map (,i) supportingIndices ++ acc

    step sortedPoints []

solve inputFilename = do
    inputPoints <- fmap readLine . T.lines <$> T.readFile inputFilename
    supportGraph <- process1 inputPoints
    let
        nPoints = length inputPoints
        makeSupportArray sg = V.map V.fromList supportListVector
            where
                supportListVector = V.accum (flip (:)) (V.replicate nPoints []) sg
        supportsVector = makeSupportArray supportGraph
        supportedByVector = makeSupportArray (map (\(a, b) -> (b, a)) supportGraph)
        removable i = all (\supported -> V.length (supportedByVector V.! supported) > 1) (supportsVector V.! i)
        nRemovable = length . filter removable $ [0..nPoints - 1]
    return nRemovable

-- >>> solve "inputs/sample/22.txt"
-- 5

-- >>> solve "inputs/real/22.txt"
-- 391
