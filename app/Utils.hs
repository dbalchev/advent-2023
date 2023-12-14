module Utils where
import           Data.List   (transpose)
import qualified Data.Vector as V

vTranspose = V.fromList . (V.fromList <$>) . transpose . (V.toList <$>) . V.toList
