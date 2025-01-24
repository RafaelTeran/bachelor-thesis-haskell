-- | Module: DataBaseGenerator
-- | Description: Create a database of introductions from Wikipedia articles.
-- | Functions:
-- | - insertArticleIntoDatabase: Fetches a Wikipedia article by its name and insert it into the database.
-- | - populateRandomDatabase: Create a database and populate it with random Wikipedia articles.
-- | - extractColumn: Extract a specific column from query results.
-- | - queryDatabase: Execute a query on the database and return the results.

module DataBaseGenerator where

import TextGenerator       -- Importing TextGenerator for Wikipedia text fetching.
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Data.Either

-- Function to create a new SQLite database and a table to store Wikipedia data
createDatabase :: String -> String -> IO ()
createDatabase file tableName = do
  conn <- connectSqlite3 file
  run conn createStatement []
  commit conn
  disconnect conn
  putStrLn "Database successfully created."
  where 
    createStatement = "CREATE TABLE " ++ tableName ++ 
                      " (language TEXT, name TEXT, text TEXT)"

-- Function to insert a single Wikipedia article into the database
insertTextIntoDatabase :: Wiki -> String -> String -> IO ()
insertTextIntoDatabase wiki file tableName = do
  conn <- connectSqlite3 file
  stmt <- prepare conn insertStatement
  execute stmt [toSql (language wiki), toSql (name wiki), toSql (text wiki)]
  commit conn
  disconnect conn
  where 
    insertStatement = "INSERT INTO " ++ tableName ++ " VALUES (?, ?, ?)"

-- Function to fetch a Wikipedia article by its name and insert it into the database
insertArticleIntoDatabase :: (String, String) -> String -> String -> IO ()
insertArticleIntoDatabase article file tableName = do
  conn <- connectSqlite3 file
  stmt <- prepare conn insertStatement
  maybeWiki <- extractIntroByTitle article
  case maybeWiki of
      Left error -> putStrLn "Text not found"
      Right wiki -> do
        execute stmt [toSql (language wiki), toSql (name wiki), toSql (text wiki)]
        commit conn
        disconnect conn
        putStrLn "Successfully inserted into the database."
  where 
    insertStatement = "INSERT INTO " ++ tableName ++ " VALUES (?, ?, ?)"

-- Function to create a database and populate it with random Wikipedia articles
populateRandomDatabase :: String -> String -> Int -> [String] -> IO ()
populateRandomDatabase file tableName numArticles languages = do
  texts <- randomTexts numArticles languages
  createDatabase file tableName
  mapM_ (\text -> either putStrLn 
        (\x -> insertTextIntoDatabase x file tableName) text) texts
  let successful = length (rights texts)
  putStrLn $ "Successfully inserted " ++ show successful ++ " entries into the database."

-- Helper function to extract a specific column from query results
extractColumn :: [[SqlValue]] -> Integer -> [String]
extractColumn sql columnIndex =
  map (\row -> fromSql $ genericIndex row columnIndex :: String) sql

-- Function to execute a query on the database and return the results
queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase file query = do
  conn <- connectSqlite3 file
  result <- quickQuery' conn query []
  disconnect conn
  return result
