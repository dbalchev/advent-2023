{-# LANGUAGE OverloadedStrings #-}
module Day15 where
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import           Control.Monad       (forM_)
import           Data.Bool           (bool)
import           Data.Char           (ord)

aocHash = foldl (\old -> (`mod` 256). (17 *) . (old +) ) 0 . map ord

-- >>> aocHash "HASH"
-- 52

data Instruction = Remove T.Text | Set T.Text Int
    deriving (Show, Eq)

parseInstruction instructionText
    | T.last instructionText == '-' = Remove (T.init instructionText)
    | otherwise                     = Set label (read $ T.unpack focalLengthText)
    where
        [label, focalLengthText] = T.splitOn "=" instructionText

-- >>> parseInstruction "rn=1"
-- Set "rn" 1

-- >>> parseInstruction "cm-"
-- Remove "cm"

applyInstruction (Remove label) lens = maybe lens dropLens $ V.findIndex ((==label) . fst) lens
    where
        dropLens lensIndex = V.take lensIndex lens V.++ V.drop (lensIndex + 1) lens

applyInstruction (Set label focalLength) lens = maybe appended setLens $ V.findIndex ((==label) . fst) lens
    where
        appended = V.snoc lens (label, focalLength)
        setLens lensIndex = lens V.// [(lensIndex, (label, focalLength))]

labelOf (Remove label) = label
labelOf (Set label _)  = label

evalInstruction boxes instruction = do
    let boxIndex = aocHash . T.unpack . labelOf $ instruction
    MV.modify boxes (applyInstruction instruction) boxIndex

focusingPower boxes = sum $ do
    (boxNo, lenses) <- zip [1..] (V.toList boxes)
    (slotNo, (_, focalLength)) <- zip [1..] (V.toList lenses)
    return $ boxNo * slotNo * focalLength


solve inputFilename = do
    inputSequence <- T.splitOn "," . T.filter (/= '\n') <$> T.readFile inputFilename
    let solution1 = sum . fmap (aocHash . T.unpack) $ inputSequence
    boxes <- MV.replicate 256 V.empty
    forM_ (parseInstruction <$> inputSequence) (evalInstruction boxes)
    boxesState <- V.freeze boxes
    let solution2 = focusingPower boxesState
    return (solution1, solution2)

-- >>> solve "inputs/sample/15.txt"
-- (1320,145)

-- >>> solve "inputs/real/15.txt"
-- (505427,243747)
