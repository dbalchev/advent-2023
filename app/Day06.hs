module Day06 where

import           Control.Monad (when)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import           Data.Vector   (Vector)
import qualified Data.Vector   as DV
import           GHC.Float     (int2Double)

pairTwo :: [[Int]] -> [(Int, Int)]
pairTwo (a:b:_) = zip a b

solveOne time recordDistance = do
    -- t * (time - t) > recordDistance
    -- -t ^ 2 + time * t- recordDistance > 0
    let ftime = int2Double time
    let frd = int2Double recordDistance
    let dis = ftime * ftime - 4 * frd
    when (dis < 0) $ do
        Left "Negative dis"
    when (dis == 0) $ do
        Left "Zero dis"
    let sqDis = sqrt dis
    let t1 = (ftime - sqDis) * 0.5
    let t2 = (ftime + sqDis) * 0.5
    let it1 = ceiling t1
    let it2 = floor t2
    let good t = t * (time - t) > recordDistance && t >= 0 && t <= time
    let goodIt1 = filter good [it1 - 1, it1, it1 + 1]
    let goodIt2 = filter good [it2 + 1, it2, it2 - 1]
    when (null goodIt1) $ do
        Left $ "empty goodIt1 " ++ show (t1, t2)
    when (null goodIt2) $ do
        Left "empty goodIt2"
    -- Left $ "foo " ++ show goodIt1 ++ show goodIt2
    return $ head goodIt2 - head goodIt1 + 1

-- >>> solveOne 7 9
-- Right 4

solve inputFilename = do
    inputString <- T.readFile inputFilename
    let input1 = DV.fromList . pairTwo . (((read . T.unpack) <$>) . drop 1 <$> T.words <$>) . T.lines $ inputString
    let solution1 = (fmap product . traverse (uncurry solveOne) . DV.toList) input1
    let input2 = head . pairTwo . map (:[]) . (read . concatMap T.unpack . drop 1 <$> T.words <$>) . T.lines $ inputString
    let solution2 = uncurry solveOne input2
    return (solution1, solution2)


-- >>> solve "inputs/sample/06.txt"
-- (Right 288,Right 71503)

-- >>> solve "inputs/real/06.txt"
-- (Right 220320,Right 34454850)
