module Day07 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.List (elemIndex, group, sort, sortOn, partition, uncons)
import Data.Ord (Down(Down))

rankedCards1 = reverse "AKQJT98765432"
rankedCards2 = reverse "AKQT98765432J"

firstTwo :: [T.Text] -> (String, Int)
firstTwo (a:b:_) = (T.unpack a, read . T.unpack $ b)

genericSolve rankedCards processGroups parsedInput = sum $ zipWith (*) (snd <$> sortedHands) [1..] 
    where
        Just cardRanks = traverse (traverse (`elemIndex` rankedCards) . fst ). V.toList $ parsedInput
        bets = snd <$> V.toList parsedInput
        rankGroups = sortOn Down . (length <$>) . processGroups . group . sort <$> cardRanks
        sortedHands = sortOn fst $ zip (zip rankGroups cardRanks) bets

processGroups2 :: [[Int]] -> [[Int]]
processGroups2 groups = if (not . null) sortedNj then (firsNj ++ jGroup) : restNj else groups
    where
        (jGroups, nonJGroups) = partition ((==0) . head) groups
        jGroup = maybe [] fst (uncons jGroups)
        sortedNj = sortOn (Down . length) nonJGroups
        firsNj:restNj = sortedNj

-- >>>processGroups2 [[1, 1, 1], [2, 2]]
-- [[1,1,1],[2,2]]

-- >>>processGroups2 [[0, 0],[2], [1, 1, 1]]
-- [[1,1,1,0,0],[2]]

solve inputFilename = do
    inputText <- T.readFile inputFilename
    let parsedInput = V.fromList . ( firstTwo . T.words <$>) . T.lines $ inputText
    let solution1 = genericSolve rankedCards1 id parsedInput
    let solution2 = genericSolve rankedCards2 processGroups2 parsedInput
    return (solution1, solution2)

-- >>> solve "inputs/sample/07.txt"
-- (6440,5905)

-- >>> solve "inputs/real/07.txt"
-- (250951660,251481660)
