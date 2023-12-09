module Day09 where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V

extrapolate ::  V.Vector Int -> (Int, Int)
extrapolate values
    | all (== 0) values = (0, 0)
    | otherwise         = (V.head values - edStart, V.last values + edEnd)
    where
        deltas = V.zipWith (-) (V.tail values) values
        (edStart, edEnd) = extrapolate deltas

solve inputFilename = do
    input <- V.fromList . ( V.fromList . (read . T.unpack <$>) . T.words <$>) . T.lines <$> T.readFile inputFilename
    let extrapolated = extrapolate <$> input
    let solution1 = V.sum . (snd <$>) $ extrapolated
    let solution2 = V.sum . (fst <$>) $ extrapolated
    return (solution1, solution2)

-- >>> solve "inputs/sample/09.txt"
-- (114,2)

-- >>> solve "inputs/real/09.txt"
-- (2098530125,1016)

