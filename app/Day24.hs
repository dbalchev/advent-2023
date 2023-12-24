{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Data.Ratio ((%))
import Data.List (group)
import Data.Either (rights)
import Control.Monad (guard)
import Utils (pairs)
import Data.Bool (bool)

parseLine line = tokens
    where
        tokens :: [[Integer]]
        tokens = fmap ((read . T.unpack <$>) . T.splitOn ", ") . T.splitOn "@" $ line

doesIntersect (testAreaStart, testAreaEnd) [[px1, py1, _], [vx1, vy1, _]] [[px2, py2, _], [vx2, vy2, _]] = result
    where
        -- px1 + t1 * vx1 == px2 + t2 * vx2
        -- py1 + t1 * vy1 == py2 + t2 * vy2

        -- (vx1) * t1 + (-vx2) * t2 = px2 - px1
        -- (vy1) * t1 + (-vy2) * t2 = py2 - py1
        detden = vy1 * vx2 - vx1 * vy2
        dx = px2 - px1
        dy = py2 - py1
        det1 = vx2 * dy - vy2 * dx
        det2 = vx1 * dy - vy1 * dx

        t1 = det1 % detden
        t2 = det2 % detden
        cx = fromInteger px1 + t1 * fromInteger vx1
        cy = fromInteger py1 + t1 * fromInteger vy1
        result = detden /= 0 && t1 > 0 && t2 > 0 && all (\a -> a >= testAreaStart && a <= testAreaEnd) [cx, cy]

-- >>> doesIntersect (7, 27) [[19,13,30],[-2,1,-2]] [[18,19,22],[-1,-1,-2]]
-- True


-- >>> doesIntersect (7, 27) [[19,13,30],[-2,1,-2]] [[12,31,28],[-1,-2,-1]]
-- False


solve testArea inputFilename = do
    hails <- fmap parseLine . T.lines <$> T.readFile inputFilename
    let 
        r1 = sum $ map (bool 0 1 . uncurry (doesIntersect testArea)) $ pairs hails
    return r1

-- >>> solve (7,27) "inputs/sample/24.txt"
-- 2

-- >>> solve (200000000000000,400000000000000) "inputs/real/24.txt"
-- 13754
