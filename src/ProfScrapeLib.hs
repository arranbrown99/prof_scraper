{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Text.HTML.Scalpel
import Data.List

appendURL :: String -> String
-- function which takes in a school ie computing and 
-- appends it into the url ie https://www.gla.ac.uk/schools/computing/staff/
appendURL school = "https://www.gla.ac.uk/schools/" ++ school ++ "/staff"


scrapeProfDiv :: Scraper String [String]
-- scrapes for the Div tag that contains proffessor
-- outputs a list containing all of the proffessors
scrapeProfDiv = chroot ("div" @: ["id" @= "content_1234567"]) scrapeNames


scrapeHonorary :: Scraper String [String]
-- Scrapes for div that contains the honorary or visiting
scrapeHonorary = chroot ("div" @: ["data-url-hash" @= "honorary-visiting"]) scrapeNames


scrapeNames :: Scraper String [String]
-- Scrapes for the names of all people not just professors
scrapeNames = texts "a"


scrapeAllItems :: String -> (Scraper String [String]) -> IO (Maybe [String])
--returns a list containing all list elements in a string
-- proagates through Nothing if the Url is invalid
scrapeAllItems input scrapeFunction = scrapeURL input scrapeFunction 
      
profList :: [String] -> [String]
--takes in a list of strings and returns a list of strings that contain professor
profList (x:xs) = (if (isInfixOf "Professor" x) then (x:profList xs) else (profList xs))
profList [] = []


maybeSubtract :: Maybe Int -> Maybe Int -> Maybe Int
-- subtracts the values inside 2 maybe ints
maybeSubtract x y = do
  xUnpacked <- x
  yUnpacked <- y
  return (xUnpacked - yUnpacked)

count :: IO (Maybe [String]) -> IO (Maybe [String]) -> IO (Maybe Int)
-- counts the number of proffessors in total minus those from the honorary and visiting section
count allList honoraryList = do
  -- unpack IO into Maybe [String]
  allMaybeList <- allList
  honoraryMaybeList <- honoraryList

  -- filter list of names into list of professor names
  let filteredAllList = (profList <$> allMaybeList)
  let filteredHonoraryList = (profList <$> honoraryMaybeList)
  return (maybeSubtract (length <$> filteredAllList) (length <$> filteredHonoraryList))
  


numProfessors :: String -> IO (Maybe Int)
numProfessors x = count allProfList honoraryList
  where
  allProfList = scrapeAllItems (appendURL x) scrapeProfDiv
  honoraryList = scrapeAllItems (appendURL x) scrapeHonorary
