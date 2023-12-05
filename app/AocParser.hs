module AocParser where
import qualified Text.Parsec.Language as TPL
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as TPT

intParser :: Parser Int
intParser = fromEnum <$> TPT.integer TPL.haskell
