{-# LANGUAGE OverloadedStrings #-}

module Day19 where


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST (runST)
import Control.Monad (forM_, when)
import GHC.ST (ST(ST))
import Data.Bool (bool)
import Data.Maybe (isNothing)

data Part = Part {getX :: Int, getM :: Int, getA :: Int, getS :: Int}
    deriving Show

data Instruction next = Accept | Reject | Conditional (Char, Ordering, Int) next next
    deriving Show

newtype ParseInstruction = ParseInstruction (Instruction (Either T.Text ParseInstruction))
    deriving Show


newtype CompiledInstruction = CompiledInstruction (Instruction CompiledInstruction)

parseInstruction line = (T.take openBraceIndex line, toInstruction ops)
    where
        Just openBraceIndex = T.findIndex (=='{') line
        ops = T.splitOn "," . T.dropEnd 1 . T.drop (openBraceIndex + 1) $ line
        parseOp op = ParseInstruction . Conditional (T.head condition, parseOrd $ T.index condition 1,  num) (Left nextIfTrue)
            where
                [condition, nextIfTrue] = T.splitOn ":" op
                parseOrd '<' = LT
                parseOrd '>' = GT
                num = read . T.unpack . T.drop 2 $ condition :: Int
        toInstruction [x, y] = parseOp x (Left y)
        toInstruction (x:xs) = parseOp x (Right $ toInstruction xs)
            where
                [condition, nextIfTrue] = T.splitOn ":" x

-- >>> parseInstruction "px{a<2006:qkq,m>2090:A,rfg}"
-- ("px",ParseInstruction (Conditional ('a',LT,2006) (Left "qkq") (Right (ParseInstruction (Conditional ('m',GT,2090) (Left "A") (Left "rfg"))))))

parseWorkflow lines = runST $ do
    let parsedInstructions = parseInstruction <$> lines
    ht <- HT.new
    forM_ parsedInstructions (uncurry (HT.insert ht))
    let
        compileI Accept = return . CompiledInstruction $ Accept
        compileI Reject = return . CompiledInstruction $ Reject
        compileI (Conditional a n1 n2) = do
            cn1 <- compile n1
            cn2 <- compile n2
            return $ CompiledInstruction (Conditional a cn1 cn2)
        compile (Left "A") = return $ CompiledInstruction Accept
        compile (Left "R") = return $ CompiledInstruction Reject
        compile (Left text) = do
            jpi <- HT.lookup ht text
            let pi = maybe (error $ "fail for " ++ T.unpack text) id jpi
            compile $ Right pi
        compile (Right (ParseInstruction pi)) = compileI pi
    compile (Left "in")

parsePart line = eqs
    where
        parseEq eq = (T.head eq, read . T.unpack . T.drop 2 $ eq :: Int)
        eqs = map parseEq . T.splitOn "," . T.drop 1 . T.dropEnd 1 $ line

evaluateWorkflow (CompiledInstruction Accept) _ = True
evaluateWorkflow (CompiledInstruction Reject) _ = False
evaluateWorkflow (CompiledInstruction (Conditional (field, comp, compValue) ifTrue ifFalse)) part
    = evaluateWorkflow (bool ifFalse ifTrue branch) part
    where
        Just partValue = lookup field part
        branch = compare partValue compValue == comp

-- >>> parsePart "{x=787,m=2655,a=1222,s=2876}"
-- [('x',787),('m',2655),('a',1222),('s',2876)]

solve inputFilename = do
    fileContent <- T.readFile inputFilename
    let (instructionLines, _:partsLines) = break T.null $ T.lines fileContent
    let workflow = parseWorkflow instructionLines
    let parts = parsePart <$> partsLines
    let passingParts = filter (evaluateWorkflow workflow) parts
    let solution1 = sum . concatMap (fmap snd) $ passingParts 
    return (solution1)

-- >>> solve "inputs/sample/19.txt"
-- 19114

-- >>> solve "inputs/real/19.txt"
-- 432427
