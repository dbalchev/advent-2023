module Day15 where
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Data.Char    (ord)

aocHash = foldl (\old -> (`mod` 256). (17 *) . (old +) ) 0 . map ord

-- >>> aocHash "HASH"
-- 52

solve inputFilename = do
    inputSequence <- T.splitOn (T.pack ",") <$> T.filter (/= '\n') <$> T.readFile inputFilename
    let solution1 = sum . fmap (aocHash . T.unpack) $ inputSequence
    return solution1

-- >>> solve "inputs/sample/15.txt"
-- 1320

-- >>> solve "inputs/real/15.txt"
-- 505427
