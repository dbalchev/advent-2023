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
import Data.Semigroup (Arg(Arg))
import Data.Maybe (catMaybes)

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

-- rpx + ti * rvx = pxi + ti * vxi
-- rpy + ti * rvy = pyi + ti * vyi
-- rpz + ti * rvz = pzi + ti * vzi

-- ti * (rvc - vci) = pci - rpc
-- ti = (pci - rpc) / (rvc - vci)

-- (px_i - rpx) * (rvy - vy_i) = (py_i - rpy) * (rvx - vx_i)
-- (py_i - rpy) * (rvz - vz_i) = (pz_i - rpz) * (rvy - vy_i)
-- (pz_i - rpz) * (rvx - vx_i) = (px_i - rpx) * (rvz - vz_i)


-- (rpx - px_i) * (rvy - vy_i) = (rpy - py_i) * (rvx - vx_i)
-- (rpy - py_i) * (rvz - vz_i) = (rpz - pz_i) * (rvy - vy_i)
-- (rpz - pz_i) * (rvx - vx_i) = (rpx - px_i) * (rvz - vz_i)

computeScore hails parameterVector = sum $ map score hails
    where
        [rpx, rpy, rpz, rvx, rvy, rvz] = V.toList parameterVector
        score [[px, py, pz], [vx, vy, vz]] = eq1 + eq2 + eq3 + if t < 0 then 1000 else 0
            where
                eq1 = (rpx - px) * (rvy - vy) - (rpy - py) * (rvx - vx)
                eq2 = (rpy - py) * (rvz - vz) - (rpz - pz) * (rvy - vy)
                eq3 = (rpz - pz) * (rvx - vx) - (rpx - px) * (rvz - vz)
                tx = if rvx - vx /= 0 then Just $ (px - rpx) % (rvx - vx) else Nothing
                ty = if rvy - vy /= 0 then Just $ (py - rpy) % (rvy - vy) else Nothing
                tz = if rvz - vz /= 0 then Just $ (pz - rpz) % (rvz - vz) else Nothing
                t = head $ catMaybes [tx, ty, tz, Just $ -1]

refine hails parameterVector (low, high) i
    | high - low < 5  = parameterVector V.// [(i, bestX)]
    | sm <= 0 && smm1 < smp1 || (sm >=0 && smm1 > smp1)  = refine hails parameterVector (mid, high) i
    | otherwise       = refine hails parameterVector (low, mid) i
    where
        score x = computeScore hails (parameterVector V.// [(i, x)])
        mid = (low + high) `div` 2
        [smm1, sm, smp1] = map score [mid - 1, mid, mid + 1]
        Arg _ bestX = minimum [ Arg (abs $ score x) x | x <- [low..high], x /=0]


refineN hails parameterVector bounds i n
    | i == n = [parameterVector]
    | i < n  = parameterVector : refineN hails (refine hails parameterVector bounds (i `mod` 6)) bounds (i + 1) n

solve testArea inputFilename = do
    hails <- fmap parseLine . T.lines <$> T.readFile inputFilename
    let
        r1 = sum $ map (bool 0 1 . uncurry (doesIntersect testArea)) $ pairs hails
        -- refined = computeScore hails $ V.fromList[24, 13, 10, -3, 1, 2]
        initialParams = V.replicate 6 0
        refined = drop 990 $ refineN hails initialParams (-1000, 1000) 0 1000

    return (r1, refined, computeScore hails $ head refined)

-- >>> solve (7,27) "inputs/sample/24.txt"
-- (2,[[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1],[-333,636,1,1,-1,-1]],-1)

-- >>> solve (200000000000000,400000000000000) "inputs/real/24.txt"
-- 13754
