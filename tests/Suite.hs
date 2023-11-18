module Main where
import           Day01_test (day01Suite)
import           Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual,
                             runTestTTAndExit)

allSuites = TestList [day01Suite]
main :: IO ()
main = do
    runTestTTAndExit allSuites
