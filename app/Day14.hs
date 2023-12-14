module Day14 where
import           AocParser   (readCharMap)
import qualified Data.Vector as V
import           Utils       (vTranspose)
import Data.List (groupBy, sort, sortOn)
import Data.Ord (Down(Down))
import Data.Bool (bool)


fallLeft = V.fromList . concatMap (sortOn Down)  . groupBy (\a b -> a /= '#' && b /= '#') . V.toList

score v =  sum $ scorePixel <$> zip [0..] (V.toList v)
    where
        scorePixel (i, c) = bool 0 (V.length v - i) (c == 'O')


solve inputFilename = do
    rockMap <- readCharMap inputFilename
    let foo = 3
    let transposedMap = vTranspose rockMap
    let processedFallMap = fallLeft <$> transposedMap
    let solution1 = sum $ score <$> V.toList processedFallMap

    return solution1

-- >>> solve "inputs/sample/14.txt"
-- 136

-- >>> solve "inputs/real/14.txt"
-- 105208
