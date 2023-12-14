{-# LANGUAGE TupleSections #-}
module Day14 where
import           AocParser   (readCharMap)
import qualified Data.Vector as V
import           Utils       (vTranspose)
import Data.List (groupBy, sort, sortOn)
import Data.Ord (Down(Down))
import Data.Bool (bool)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.Trans.Except (throwE, runExceptT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad (when)


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

toHashable = fmap V.toList . V.toList

northBeamScore = sum . fmap score . V.toList . fmap fallLeft . vTranspose

solve inputFilename = do
    rockMap <- readCharMap inputFilename
    let solution1 = northBeamScore rockMap
    let after1 = spinCycle rockMap
    let limit = 1000
    let targetStep = 1000000000 
    let solution2 = runST $ do
        stateToStep <- HT.new
        let oneStep stepNo currentState oldStateCache = do
            let key = toHashable currentState
            let stateCache = currentState : oldStateCache
            lookupResult <- lift $ HT.lookup stateToStep key
            maybe (return ()) (throwE . (,stepNo, stateCache)) lookupResult
            when (stepNo >= limit) $ throwE (-1, -1, [])
            lift $ HT.insert stateToStep key stepNo
            oneStep (stepNo + 1) (spinCycle currentState) stateCache
        Left (cycleStart, cycleEnd, stateCache) <- runExceptT $ oneStep 0 rockMap []
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
