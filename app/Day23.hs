module Day23 where
import           AocParser   (readCharMap)
import qualified Data.Vector as V


solve inputFilename = do
    hikeMap <- readCharMap inputFilename

    let 
        nRows = V.length hikeMap
        nCols = V.length (V.head hikeMap)
        Just entryCol = V.findIndex (== '.') (V.head hikeMap)
        Just exitCol = V.findIndex (== '.') (V.last hikeMap)
        
    return (entryCol, exitCol)

-- >>> solve "inputs/sample/23.txt"
-- (1,21)
