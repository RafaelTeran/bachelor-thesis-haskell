-- | Module: LinkGenerator
-- | Description: Generates random Wikipedia article links for specified languages.
-- | Functions:
-- | - randomLinksByLanguage: Fetches random articles for a single language.
-- | - randomLinks: Fetches random articles for multiple languages, handling limits.

module LinkGenerator (
    randomLinksByLanguage,
    randomLinks
    ) where

import Prelude hiding (id)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import GHC.Generics

-- Define types for handling API responses
data RandomArticle = RandomArticle
    { id :: Int -- The unique identifier of the article.
    } deriving (Show, Generic) 

data RandomSearch = RandomSearch
    { random :: [RandomArticle] -- List of random articles.
    } deriving (Show, Generic)

data ApiResponse = ApiResponse
    { query :: RandomSearch -- Query containing the random articles.
    } deriving (Show, Generic)

-- JSON parsing instances for the data types
instance FromJSON RandomArticle
instance FromJSON RandomSearch
instance FromJSON ApiResponse

-- Fetches a specified number of random article links for a specific language.
randomLinksByLanguage :: Int -> String -> IO [(String, String)]
randomLinksByLanguage nArticles language = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ language ++ ".wikipedia.org/w/api.php?action=query" ++
              "&format=json&list=random&rnnamespace=0&rnlimit=" ++ show nArticles
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeResponse = decode (responseBody res) :: Maybe ApiResponse
    return $ case maybeResponse of
        Just apiResponse ->
            fmap (\page -> (language, show (id page))) (random (query apiResponse))
            -- Extracts the language and article IDs from the response.
        Nothing -> [] -- Return an empty list if the response cannot be parsed.

-- Fetches a specified number of random article links for multiple languages.
randomLinks :: Int -> [String] -> IO [(String, String)]
randomLinks nArticles languages = 
    if nArticles > 500 -- Automatically handles requests exceeding the 500-article limit.
    then do
        -- If the number of articles exceeds 500, make recursive calls.
        rest <- randomLinks (nArticles - 500) languages
        auxResult <- mapM (\x -> randomLinksByLanguage 500 x) languages
        return (concat auxResult ++ rest)
    else do
        -- Otherwise, fetch articles in a single request per language.
        result <- mapM (\x -> randomLinksByLanguage nArticles x) languages
        return (concat result)
