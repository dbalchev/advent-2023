{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Day01_test where
import qualified Data.Vector as DV
import           Day01       (inputParser)
import           Test.HUnit  (Test (TestCase, TestLabel, TestList), assertEqual)
import           Text.Parsec (parse)

parseTest = TestCase $ do
    let actual = parse inputParser "" "1234\n4567\n\n12\n34"
    let expected = Right [[1234, 4567],[12, 34]]
    assertEqual "parsed result is different" actual expected

day01Suite = TestList [TestLabel "parseTest" parseTest]
