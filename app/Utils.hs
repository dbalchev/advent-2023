{-# LANGUAGE TupleSections #-}
module Utils where
import           Control.Monad (guard)
import           Data.List     (tails, transpose)
import qualified Data.Vector   as V

vTranspose = V.fromList . (V.fromList <$>) . transpose . (V.toList <$>) . V.toList

pairs list = do
    t <- tails list
    guard $ (not . null) t
    let (x : xs) = t
    map (x,) xs

-- >>> pairs "abcd"
-- [('a','b'),('a','c'),('a','d'),('b','c'),('b','d'),('c','d')]
