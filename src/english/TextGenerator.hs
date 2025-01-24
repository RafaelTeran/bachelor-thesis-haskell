-- | Module: TextGenerator
-- | Description: Download the introductory text of Wikipedia articles in plain text.
-- | Functions:
-- | - randomTexts: Fetches random texts from Wikipedia.
-- | - extractIntroByTitle: Fetches an article's intro by its title.

module TextGenerator (
    Wiki,                -- Data type representing a Wikipedia article.
    language,            -- Language code of the article.
    name,                -- Title of the article.
    text,                -- Cleaned introductory text of the article.
    randomTexts,
    extractIntroByTitle
) where

import LinkGenerator      -- Importing the LinkGenerator module for random link generation.
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Char

-- Define types for handling API responses
data ApiResponse = ApiResponse
     { query :: SearchResult -- Query results from the Wikipedia API.
     } deriving (Show, Generic)

data SearchResult = SearchResult
     { pages :: HM.HashMap T.Text Article -- A map of articles keyed by their page IDs.
     } deriving (Show, Generic)

data Article = Article
     { title :: !T.Text,  -- Title of the article.
       extract :: !T.Text -- Extracted intro text of the article.
     } deriving (Show, Generic)

-- Define a custom type representing a Wikipedia article in our domain
data Wiki = Wiki
     { language :: !T.Text, -- Language code of the article.
       name    :: !T.Text,  -- Title of the article.
       text     :: !T.Text  -- Cleaned introductory text of the article.
     } deriving (Show, Generic)

-- Enable JSON decoding for the defined types
instance FromJSON ApiResponse
instance FromJSON SearchResult
instance FromJSON Article

-- Fetch the introductory text of an article using its page ID and language
extractIntro :: (String, String) -> IO (Either String Wiki)
extractIntro (language, articleID) = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ language ++ 
              ".wikipedia.org/w/api.php?action=query&prop=info|extracts" ++
              "&explaintext&exintro&format=json&pageids=" ++ articleID
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeApiResponse = decode (responseBody res) :: Maybe ApiResponse
    return $ case maybeApiResponse of
        Just apiResponse ->
            let article = Prelude.head $ HM.elems $ pages $ query apiResponse
                rawText = extract article
                cleanedText = cleanIntro rawText
                articleTitle = title article
            in Right (Wiki (T.pack language) articleTitle cleanedText)
        Nothing -> Left "Text not found"

-- Fetch the introductory text of an article using its title and language
extractIntroByTitle :: (String, String) -> IO (Either String Wiki)
extractIntroByTitle (language, articleTitle) = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ language ++ 
              ".wikipedia.org/w/api.php?action=query&prop=info|extracts" ++
              "&explaintext&exintro&format=json&titles=" ++ articleTitle
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeApiResponse = decode (responseBody res) :: Maybe ApiResponse
    return $ case maybeApiResponse of
        Just apiResponse ->
            let article = Prelude.head $ HM.elems $ pages $ query apiResponse
                rawText = extract article
                cleanedText = cleanIntro rawText
                finalTitle = title article
            in Right (Wiki (T.pack language) finalTitle cleanedText)
        Nothing -> Left "Text not found"

-- Clean introductory text by removing newlines, converting to lowercase, and keeping only alphabetic characters and spaces
cleanIntro :: T.Text -> T.Text
cleanIntro text =
    let cleanText = T.replace (T.pack "\n") (T.pack " ") text
    in T.pack (Prelude.map toLower (Prelude.filter (\c -> isAlpha c || c == ' ') 
                                    (T.unpack cleanText)))

-- Fetch random texts from Wikipedia given the number of articles and languages
randomTexts :: Int -> [String] -> IO [Either String Wiki]
randomTexts nArticles languages = do
    links <- randomLinks nArticles languages -- Use LinkGenerator's function to get random links
    mapM extractIntro links -- Fetch the intro for each link
