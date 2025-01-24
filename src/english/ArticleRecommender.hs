-- | Module: LanguageClassifier
-- | Description: Application of a Wikipedia article recommender using PCA.
-- | Functions:
-- | - articleRecommender: Recommends Wikipedias articles based on a given title.

module ArticleRecommender where

import DataBaseGenerator  -- Importing DataBaseGenerator for creating a random Wikipedia database.
import Recommender        -- Importing Recommender for using the PCA recommender.
import Data.List as L
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC

-- Generate a database with a specified number of random intros for a specified language.
--            (The database is been named "articleIntros.sql", up to change from the user)
--            (Its table is been named "articlesTable", also up to change)
articleIntros :: Int -> String -> IO ()
articleIntros nIntros language = do
   populateRandomDatabase "articleIntros.sql" "articlesTable" nIntros [language]

-- Inserts a single article by its name into the created database.
insertArticle :: (String, String) -> IO ()
insertArticle article = do
   insertArticleIntoDatabase article "articleIntros.sql" "articlesTable"

-- Fetch intros from the database and return them as pairs (name, text).
titleIntros :: IO [(String, String)]
titleIntros = do
   dbData <- queryDatabase "articleIntros.sql" "SELECT name, text FROM articlesTable"
   let intros = zip (extractColumn dbData 0) (extractColumn dbData 1)
   return $ HM.keys $ frequency intros

-- List of stopwords in English (up to changes from the user)
stopWords :: [String]
stopWords = ["a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves"]

-- Recommends an specified number of Wikipedia articles based on a given title.
articleRecommender :: String -> Int -> IO [String]
articleRecommender title n = do
  insertArticle ("en", title) -- "en" stands for English (can be changed for other language)
  dataEntries <- titleIntros
  return $ recommender dataEntries stopWords title n 50 0.9
  -- number 50 stands for the number of keywords taken into consideration for calculations (up to change)
  -- number 0.9 stands for the percentage of total variance explain (at least 90%) (up to change)

