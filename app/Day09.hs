module Day09 where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

extrapolate ::  V.Vector Int -> Int
extrapolate values
    | all (== 0) values = 0
    | otherwise         = V.last values + extrapolatedDelta 
    where
        deltas = V.zipWith (-) (V.tail values) values
        extrapolatedDelta = extrapolate deltas

solve inputFilename = do
    input <- V.fromList . ( V.fromList . (read . T.unpack <$>) . T.words <$>) . T.lines <$> T.readFile inputFilename
    let extrapolated = extrapolate <$> input
    let solution1 = V.sum extrapolated
    return (solution1)

-- >>> solve "inputs/sample/09.txt"
-- 114

-- >>> solve "inputs/real/09.txt"
-- 2098530125

