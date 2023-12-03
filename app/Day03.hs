module Day03 where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit)
import qualified Data.Vector           as DV
import Control.Monad (guard)


m1 a = a - 1

solve inputFilename = do
    taskInput <- DV.fromList . filter (not . BS.null) . BS.lines <$> BS.readFile inputFilename
    let partNumbers = concat $ do
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
                
                return context
            guard $ any (BS.any ((&&) <$> (not . isDigit) <*> (/= '.'))) contextStrings
            return fullNumber
        return linePartNumbers
    let solution1 = sum . map fst <$> traverse BS.readInt partNumbers
    return (solution1, Nothing)

-- >>> solve "inputs/sample/03.txt"
-- (Just 4361,Nothing)

-- >>> solve "inputs/real/03.txt"
-- (Just 514969,Nothing)
