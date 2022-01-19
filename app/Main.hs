{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.HTML.Scalpel
import ProfScrapeLib

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector


schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]

main :: IO ()
main = do
     -- Print results
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     
     -- Encode our results
     let encodedResults = Cassava.encode results

     -- Write to a csv
     ByteString.writeFile "prof_count.csv" encodedResults
