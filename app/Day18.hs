{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day18 where
import           Control.Monad             (forM, forM_, guard, replicateM_)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT (runStateT), get, put)
import           Data.Array.IO.Internals   (IOUArray (IOUArray))
import qualified Data.Array.MArray         as MA
import           Data.Bool                 (bool)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Read            as T
import qualified Data.Vector               as V

parseLine :: T.Text -> (Char, Int, T.Text)
parseLine line = (T.head direction, (read . T.unpack) nSteps, (T.drop 2 . T.dropEnd 1) color)
    where
        [direction, nSteps, color] = T.words line

getDir 'U' = (-1, 0)
getDir 'D' = (1, 0)
getDir 'L' = (0, -1)
getDir 'R' = (0, 1)

floodFill segments = do
    ((minI, minJ), (maxI, maxJ)) <- MA.getBounds segments
    indices <- fmap fst <$> MA.getAssocs segments
    (_, (lastColor, outColors)) <- (`runStateT` (2, [])) $ do
        forM_ indices $ \(ii, ij) -> do
            initialColor <- lift $ MA.readArray segments (ii, ij)
            (currentColor, oldOut) <- get
            -- lift $ print ("initial color", ii, ij, initialColor)
            let
                fill :: Int -> Int -> IO Bool
                fill i j = do
                    let isOut = i < minI || i > maxI || j < minJ || j > maxJ
                    let go = do
                        oldColor <- MA.readArray segments (i, j)
                        -- print ("oldColor", i, j, oldColor)
                        let gogo = do
                            MA.writeArray segments (i, j) currentColor
                            outs <- forM [(-1, 0), (1, 0), (0, 1), (0, -1)] (\(di, dj) -> fill (i + di) (j + dj))
                            return $ sum (map (bool 0 1) outs) > 0
                        bool (return False) gogo (oldColor == 0)
                    -- print ("isOut", i, j, isOut)
                    bool go (return True) isOut
                doColor = do
                    isOut <- lift $ fill ii ij
                    let newOut = bool oldOut (currentColor : oldOut) isOut
                    put (currentColor + 1, newOut)
            bool (return ()) doColor (initialColor == 0)
    -- print (lastColor, outColors)
    -- forM_ [minI..maxI] $ \i -> do
    --     elements <- forM [minJ..maxJ] $ \j -> MA.readArray segments (i, j)
    --     print elements
    sum <$> forM indices (fmap (bool 1 0.(`elem` outColors)) . MA.readArray segments)

decodePart2 (_, _, h) = (decodeDir $ T.last h, number)
    where
        decodeDir '0' = 'R'
        decodeDir '1' = 'D'
        decodeDir '2' = 'L'
        decodeDir '3' = 'U'
        Right (number :: Int, _) = T.hexadecimal (T.init h)

-- >>> decodePart2 (0, 1, T.pack "70c710")
-- ('R',461937)

oArea (i1, j1) (i2, j2) = i1 * j2 - i2 * j1

dLen (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

solve inputFilename = do
    plan <- V.fromList . fmap parseLine . T.lines <$> T.readFile inputFilename
    let countDir dir = sum . map (\(a, b, _) -> bool 0 b (a == dir)) . V.toList $ plan

    (_, (_, (minI, maxI, minJ, maxJ))) <- (`runStateT` ((0, 0), (0, 0, 0, 0))) $ do
        forM_ plan $ \(dir, nSteps, _) -> do
            let (di, dj) = getDir dir
            replicateM_ nSteps $ do
                ((i, j), (minI, maxI, minJ, maxJ)) <- get
                put ((i + di, j + dj), (min i minI, max i maxI, min j minJ, max j maxJ))

    segments <- MA.newArray ((minI, minJ), (maxI, maxJ)) 0 :: IO (IOUArray (Int, Int) Int)

    (`runStateT` (0, 0)) $ do
        forM_ plan $ \(dir, nSteps, _) -> do
            let (di, dj) = getDir dir
            replicateM_ nSteps $ do
                (i, j) <- get
                lift $ MA.writeArray segments (i, j) 1
                put (i + di, j + dj)
            (i, j) <- get
            lift $ MA.writeArray segments (i, j) 1
    solution1 :: Int <- floodFill segments
    let decoded = V.map decodePart2 plan
    let
        nextV (i, j) (dir, nSteps) = (i + di * nSteps, j + dj * nSteps)
            where
                (di, dj)= getDir dir
    let p1Vertices = V.fromList . scanl nextV (0, 0) . map (\(a, b, _) -> (a, b)) . V.toList $ plan
    let p2vertices = V.fromList . scanl nextV (0, 0) . V.toList $ decoded

    let length = V.sum $ V.zipWith dLen p2vertices (V.tail p2vertices)
    let totalOArea = V.sum $ V.zipWith oArea p2vertices (V.tail p2vertices)
    let totalArea = (length + abs totalOArea + 2) `div` 2
    return (solution1, totalArea)

-- >>> solve "inputs/sample/18.txt"
-- (62,952408144115)

-- >>> solve "inputs/real/18.txt"
-- (44436,106941819907437)
