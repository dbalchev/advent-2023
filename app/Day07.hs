module Day07 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.List (elemIndex, group, sort, sortOn)
import Data.Ord (Down(Down))

rankedCards = reverse "AKQJT98765432"
cardToRank = (`elemIndex` rankedCards)
rankToCard = (rankedCards !!)

firstTwo :: [T.Text] -> (String, Int)
firstTwo (a:b:_) = (T.unpack a, read . T.unpack $ b)



solve inputFilename = do
    inputText <- T.readFile inputFilename
    let parsedInput = V.fromList . ( firstTwo . T.words <$>) . T.lines $ inputText
    let Just cardRanks = traverse (traverse cardToRank . fst ). V.toList $ parsedInput
    let bets = snd <$> V.toList parsedInput
    let rankGroups = sortOn Down . (length <$>) . group . sort <$> cardRanks
    let sortedHands = sortOn fst $ zip (zip rankGroups cardRanks) bets
    let solution1 = sum $ zipWith (*) (snd <$> sortedHands) [1..]
    return (solution1)

-- >>> solve "inputs/sample/07.txt"
-- 6440

-- >>> solve "inputs/real/07.txt"
-- 250951660
