module Day03 where

import           Control.Monad         (guard)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit)
import           Data.List             (sort, group)
import qualified Data.Vector           as DV


m1 a = a - 1

solve inputFilename = do
    taskInput <- DV.fromList . filter (not . BS.null) . BS.lines <$> BS.readFile inputFilename
    let partNumbers = DV.fromList . concat $ do
        (lineIndex, line) <- zip [0..] $ DV.toList taskInput
        let digitStartIndices = filter (maybe True (not . isDigit) . BS.indexMaybe line . m1)  $ BS.findIndices isDigit line
        let linePartNumbers = do
            startIndex <- digitStartIndices
            let fullNumber = BS.takeWhile isDigit . BS.drop startIndex $ line
            let numLen = BS.length fullNumber
            let (contextStart, contextLen) = if startIndex == 0 then (0, numLen + 1) else (startIndex - 1, numLen + 2)
            let contextStrings = do
                contextLineIndex <- [lineIndex - 1, lineIndex, lineIndex + 1]
                guard $ 0 <= contextLineIndex
                guard $ contextLineIndex < DV.length taskInput
                let context = BS.take contextLen . BS.drop contextStart . (DV.! contextLineIndex)  $ taskInput
                let starIndexes = do
                    starStart <- BS.findIndices (==  '*') context
                    return (contextLineIndex, contextStart + starStart)
                return (context, starIndexes)
            guard $ any (BS.any ((&&) <$> (not . isDigit) <*> (/= '.')) . fst) contextStrings
            let Just (parsedNumber, _) = BS.readInteger fullNumber
            return (parsedNumber, DV.fromList $ concatMap snd contextStrings)
        return linePartNumbers
    let solution1 = sum . DV.map fst $ partNumbers
    let gearLocations = map head .filter ((==2) <$> length) . group . sort $ concatMap (DV.toList . snd) $ DV.toList partNumbers
    let solution2 = sum $ do
        gearLocation <- gearLocations
        let gearPartNumbers = map fst .filter (DV.elem gearLocation .snd) . DV.toList $ partNumbers
        return $ product gearPartNumbers
    return (solution1, solution2)

-- >>> solve "inputs/sample/03.txt"
-- (4361,467835)

-- >>> solve "inputs/real/03.txt"
-- (514969,78915902)
