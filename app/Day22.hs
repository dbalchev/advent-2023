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

-- sortCriteria :: (Int, [[Int]]) -> Int
sortCriteria = minimum . map last


unique = map head . group . sort

process1 points = do
    let
        extract f = concatMap (map f) points
        minX = minimum (extract head)
        maxX = maximum (extract head)
        minY = minimum (extract (head . tail))
        maxY = maximum (extract (head . tail))

        sortedPoints = zip [0..] $ sortOn sortCriteria points

    currentTop <- MA.newArray ((minX, minY), (maxX, maxY)) (0, -1) :: IO (IOArray (Int, Int) (Int, Int))
    let
        step [] acc = return acc
        step ((i, [[x1, y1, z1], [x2, y2, z2]]):nextPoints) acc = do
            let volumePoints = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
            topState <- traverse (MA.readArray currentTop) volumePoints
            let topZ = maximum . map fst $ topState
            let supportingIndices = unique . filter (/= (-1)) . map snd . filter ((== topZ) . fst) $ topState
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
        countFalling i = do
            currentNSupport <- MA.newArray (0, nPoints - 1) 0  :: IO(IOArray Int Int)
            forM_ [0..nPoints -1] $ \j -> MA.writeArray currentNSupport j (V.length (supportedByVector V.! j))
            let
                removeBrick :: [Int] -> Int -> IO Int
                removeBrick [] acc = return acc
                removeBrick (x:xs) acc = do
                    forM_ (supportsVector V.! x) $ \j -> do
                        oldV <- MA.readArray currentNSupport j
                        MA.writeArray currentNSupport j (oldV - 1)
                    nexts <- map fst . filter ((== 0) . snd) <$> traverse (\j -> (j,) <$> MA.readArray currentNSupport j) (V.toList $ supportsVector V.! x)
                    removeBrick (nexts ++ xs) (acc + 1)
            removeBrick [i] (-1)

        -- reverseFallIndices = V.constructN (\v -> ) nPoints
    fallingCounts <- traverse countFalling [0..nPoints - 1]
    return (nRemovable, sum fallingCounts)

-- >>> solve "inputs/sample/22.txt"
-- (5,7)

-- >>> solve "inputs/real/22.txt"
-- (391,69601)
