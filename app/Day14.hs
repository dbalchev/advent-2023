{-# LANGUAGE TupleSections #-}
module Day14 where
import           AocParser                  (readCharMap)
import           Control.Monad              (when, (<=<), (>=>))
import           Control.Monad.ST           (runST)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Bool                  (bool)
import           Data.Hashable              (Hashable, hashWithSalt)
import qualified Data.HashTable.ST.Basic    as HT
import           Data.List                  (groupBy, sort, sortOn)
import           Data.Ord                   (Down (Down))
import qualified Data.Vector                as V
import           Utils                      (vTranspose)
import Data.Function (fix)


genericFall g = V.fromList . concatMap (sortOn g)  . groupBy (\a b -> a /= '#' && b /= '#') . V.toList
fallLeft = genericFall Down
fallRight = genericFall id

score v =  sum $ scorePixel <$> zip [0..] (V.toList v)
    where
        scorePixel (i, c) = bool 0 (V.length v - i) (c == 'O')

tiltNorth = vTranspose . fmap fallLeft . vTranspose
tiltWest  = fmap fallLeft
tiltSouth = vTranspose . fmap fallRight . vTranspose
tiltEast  = fmap fallRight

spinCycle = tiltEast . tiltSouth . tiltWest .tiltNorth

newtype HashableMatrix a = HashableMatrix (V.Vector (V.Vector a)) deriving Eq
unMatrix (HashableMatrix x) = x

instance (Eq a, Hashable a) => Hashable (HashableMatrix a) where
    hashWithSalt seed = hashWithSalt seed . fmap V.toList . V.toList . unMatrix

toHashable = HashableMatrix

northBeamScore = sum . fmap score . V.toList . fmap fallLeft . vTranspose

solve inputFilename = do
    rockMap <- readCharMap inputFilename
    let solution1 = northBeamScore rockMap
    let after1 = spinCycle rockMap
    let limit = 1000
    let targetStep = 1000000000
    let solution2 = runST $ do
        stateToStep <- HT.new
        Left (cycleStart, cycleEnd, stateCache) <- runExceptT $ (flip $ fix . (>=>)) (0, rockMap, []) $
             \(stepNo,currentState, oldStateCache) -> do
                let key = toHashable currentState
                let stateCache = currentState : oldStateCache
                lookupResult <- lift $ HT.lookup stateToStep key
                maybe (return ()) (throwE . (,stepNo, stateCache)) lookupResult
                when (stepNo >= limit) $ throwE (-1, -1, [])
                lift $ HT.insert stateToStep key stepNo
                return (stepNo + 1, spinCycle currentState, stateCache)
        let cycleLen = cycleEnd - cycleStart
        let stateVector = V.reverse . V.fromList $ stateCache
        let targetStepSimple = cycleStart + (targetStep - cycleStart) `mod` cycleLen
        let targetState = stateVector V.! targetStepSimple
        let result = sum . fmap score . V.toList . vTranspose $  targetState
        return result

    return (solution1, solution2)

-- >>> solve "inputs/sample/14.txt"
-- (136,64)

-- >>> solve "inputs/real/14.txt"
-- (105208,102943)
