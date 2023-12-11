module AocParser where
import qualified Text.Parsec.Language as TPL
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as TPT

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V

intParser :: Parser Int
intParser = fromEnum <$> TPT.integer TPL.haskell

readCharMap inputFilename = V.fromList . (V.fromList . T.unpack <$>) . T.lines <$> T.readFile inputFilename
