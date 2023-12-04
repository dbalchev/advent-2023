module Day04 where
import           Control.Monad        (forM_)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as DV
import qualified Data.Vector.Mutable  as DVM
import           Text.Parsec          (parse, sepBy, spaces, string)
import qualified Text.Parsec.Language as TPL
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as TPT

data Card = Card { cardIndex :: Int, winningNumbers :: Vector Int, myNumbers :: Vector Int}
    deriving Show

intParser = fromEnum <$> TPT.integer TPL.haskell

lineParser :: Parser Card
lineParser = do
    string "Card"
    spaces
    cardIndex <- intParser
    spaces
    string ":"
    winningNumbers <- DV.fromList <$> sepBy intParser spaces
    spaces
    string "|"
    myNumbers <- DV.fromList <$> sepBy intParser spaces
    return $ Card cardIndex winningNumbers myNumbers

-- >>> parse lineParser "" "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
-- Right (Card {cardIndex = 2, winningNumbers = [13,32,20,16,61], myNumbers = [61,30,68,82,17,32,24,19]})

countMatches (Card _ winningNumbers myNumbers)
    = length . filter (`DV.elem` winningNumbers) . DV.toList $ myNumbers

score card = if matches < 1 then 0 else 2 ^ (matches - 1)
    where
        matches = countMatches card

solve inputFilename = do
    Right cards <-  fmap DV.fromList . traverse (parse lineParser "") . filter (not . null) . map T.unpack . T.lines <$> T.readFile inputFilename
    let solution1 = sum $ map score . DV.toList $ cards
    repetitions <- DVM.replicate (DV.length cards) (1 :: Int)
    forM_ (zip [0..] (DV.toList cards)) $ \(i, card) -> do
        let matches = countMatches card
        cardRepetitions <- DVM.read repetitions i
        forM_ [i + 1..i + matches] (DVM.modify repetitions (+ cardRepetitions))
    endRepetitions <- DV.freeze repetitions
    let solution2 = DV.sum endRepetitions
    return (solution1, solution2)

-- >>> solve "inputs/sample/04.txt"
-- (13,30)

-- >>> solve "inputs/real/04.txt"
-- (24848,7258152)
