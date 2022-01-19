{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import ProfScrapeLib (numProfessors)

main :: IO ()
main = do
  defaultMain (testGroup "Jeremy's public ProfScrapeLib tests"
    [
      getNumProfsForSchool "computing" 16 18,
      getNumProfsForSchool "chemistry" 15 18,
      getNumProfsForSchool "physics" 29 33,
      getNumProfsForSchool "psychology" 12 14
    ])

getNumProfsForSchool :: String -> Int -> Int -> TestTree
getNumProfsForSchool school min max =
  testCase ("testing " ++ school ++ " school") $ do
    n <- numProfessors school
    assertBool ("incorrect prof count for " ++ school) $ inBounds n min max

inBounds :: Maybe Int -> Int -> Int -> Bool
inBounds Nothing _ _ = False
inBounds (Just n) min max = (min <= n) && (n <= max)
