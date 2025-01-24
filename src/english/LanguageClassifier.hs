-- | Module: LanguageClassifier
-- | Description: Application of a language classifier using Naive Bayes and a database from Wikipedia.
-- | Functions:
-- | - languageClassifier: Classifies the language of a given text.
-- | - languageClassifierAll: Returns probabilities for all languages for a given text.

module LanguageClassifier where

import DataBaseGenerator  -- Importing DataBaseGenerator for creating a random Wikipedia database.
import Classifier         -- Importing Classifier for using the Naive Bayes classifier.
import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.Char

-- Frequency of users per language.
--           (Number of active users writing in Wikipedia at moment of the creation of this script)
--           (Changing the data or adding new languages is up to the user)
userFrequencyByLanguage :: HashMap String Double
userFrequencyByLanguage = HM.fromList 
  [("en",115659),("de",16787),("fr",15990),("es",13430),("ja",12651),
   ("ru",8752),("pt",8256),("it",7203),("zh",7153),("fa",5071),
   ("pl",4253),("ar",3766),("nl",3425),("he",3276),("uk",2975),
   ("tr",2581),("id",2567),("cs",1994),("ko",1978),("sv",1955),
   ("vi",1575),("fi",1535),("hu",1407),("th",1341),("bn",1335),
   ("no",1079),("ca",967),("hi",959),("el",862),("ro",784)]

-- Generate a database with a specified number of random intros for various languages.
--            (The database is been named "languageIntros.sql", up to change from the user)
--            (Its table is been named "languageTable", also up to change)
languageIntros :: Int -> IO ()
languageIntros nIntros = do
   populateRandomDatabase "languageIntros.sql" "languageTable" nIntros languages
   where languages = HM.keys userFrequencyByLanguage
   
-- Fetch intros from the database and return them as pairs (language, text).
intros :: IO [(String, String)]
intros = do
   database <- queryDatabase 
               "languageIntros.sql" "SELECT language, text FROM languageTable"
   let introsPairs = zip (extractColumn database 0) (extractColumn database 1)
   return $ HM.keys $ frequency introsPairs
   
-- Classifies the language of a given text using functions from module Classifier.
languageClassifier :: String -> IO String
languageClassifier text = do
     dataSet <- intros
     let classes = HM.keys $ (frequency . L.map fst) dataSet
         totalWords = fromInteger $ sum $ HM.elems
                      ((frequency . concatMap words) (L.map snd dataSet))
         totalExtracts = sum $ HM.elems userFrequencyByLanguage
         probabilities = classifierClasses text dataSet userFrequencyByLanguage 
                                            classes totalWords totalExtracts
     return $ fst (L.maximumBy (comparing snd) probabilities)

-- Returns probabilities for all languages for a given text using functions from module Classifier.
languageClassifierAll :: String -> IO [(String, Double)]
languageClassifierAll text = do
   dataSet <- intros
   let classes = HM.keys $ (frequency . L.map fst) dataSet
       totalWords = fromInteger $ sum $ HM.elems
                   ((frequency . concatMap words) (L.map snd dataSet))
       totalExtracts = sum $ HM.elems userFrequencyByLanguage
       probabilities = classifierClasses (L.map toLower text) dataSet 
                        userFrequencyByLanguage classes totalWords totalExtracts
   return (L.sortBy (comparing (Down . snd)) probabilities)
