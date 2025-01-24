-- | Module: Recommender
-- | Description: Implementation of a PCA-based recommender.
-- | Functions:
-- | - recommender: PCA-based recommender based on a given name.

module Recommender where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as LA
import Data.Maybe

-- Frequency function: Calculates the frequency of elements in a list.
frequency :: (Eq k, Hashable k, Integral v) => [k] -> HashMap k v
frequency [] = HM.empty
frequency (x:xs) = HM.insertWith (+) x 1 (frequency xs)

-- Extracts a specified number of the most relevant keywords, excluding given stopwords.
keywords :: [(String, String)] -> [String] -> Int -> [(String, Integer)]
keywords dataSet stopwords numKeywords =
  let wordFreqs = frequency (concatMap words (L.map snd dataSet))
      filteredWords = L.filter (\(word, _) -> notElem word stopwords) 
                                  (HM.toList wordFreqs)
  in take numKeywords (sortBy (\(_, c1) (_, c2) -> compare c2 c1) filteredWords)

-- Constructs a matrix of (key)word frequencies for each name.
wordMatrix :: [(String, String)] -> [String] -> [String] -> [[Double]]
wordMatrix dataSet keywords names =
  let wordFreqByName = HM.fromList 
        (L.map (\(name, intro) -> (name, frequency $ words intro)) dataSet)
  in L.map (\name -> L.map 
           (\keyword -> wordFreqForName name keyword wordFreqByName) 
           keywords) names

-- Frequency of a specific word for a given name.
wordFreqForName :: String -> String ->
                   HashMap String (HashMap String Integer) -> Double    
wordFreqForName name word frequencies =
   fromIntegral $ HM.lookupDefault 0 word (frequencies HM.! name)

-- Eigen decomposition for dimensionality reduction.
eigenDecomposition :: [[Double]] -> (Vector Double, Matrix Double)
eigenDecomposition matrix =
    let vectors = fromLists matrix
        (_, covarianceMatrix) = meanCov vectors
        (eigenvalues, eigenvectors) = eigSH covarianceMatrix
    in (eigenvalues, eigenvectors)

-- Number of principal components needed to explain a given variance.
numPrincipalComponents :: Vector Double -> Double -> Int
numPrincipalComponents eigenvalues threshold =
    let totalVariance = sumElements eigenvalues
        explainedVariance = LA.toList $ cmap (/ totalVariance) eigenvalues
        cumulativeVariance = scanl1 (+) explainedVariance
    in fromMaybe 1 (L.findIndex (> threshold) cumulativeVariance)

-- Performs Principal Component Analysis.
principalComponentAnalysis :: Matrix Double -> [[Double]] -> Int -> Matrix Double
principalComponentAnalysis eigenvectors matrix numComponents =
  tr $ mul (tr keyEigenvectors) (tr vectors)
  where
    vectors = fromLists matrix
    keyEigenvectors = takeColumns numComponents eigenvectors

-- Computes squared Euclidean distances between rows of a matrix.
squaredEuclideanDistancesByRow :: Matrix Double -> Int -> Vector Double
squaredEuclideanDistancesByRow matrix row =
  sum $ toColumns squaredDifferenceMatrix
  where
    d = cols matrix
    auxiliaryMatrix = repmat (subMatrix (fromIntegral row, 0) (1, d) matrix) 
                                   (rows matrix) 1
    differenceMatrix = matrix - auxiliaryMatrix
    squaredDifferenceMatrix = differenceMatrix * differenceMatrix

-- Ordered indexes of a list based on their values.
orderedIndexes :: [Double] -> [Int]
orderedIndexes list =
   let sortedList = nub $ sort list
   in concatMap (\x -> L.elemIndices x list) sortedList

-- Recommender function: Suggests the closest items to the given name using ACP.
-- Required: -- dataSet: database consisting of a list of names and texts.
             -- stopwords: list of stopwords.
             -- name: name of the element whose recommendations will be fetch.
             -- n: number of recommedations wanted.
             -- numKeywords: number of keywords.
             -- threshold: minimum of the total variance explained.
recommender :: [(String, String)] -> [String] -> String -> Int -> Int -> 
                                     Double -> [String]
recommender dataSet stopwords name n numKeywords threshold =
   let keywordsList = L.map fst (keywords dataSet stopwords numKeywords)
       -- List of keywords
       names = keys $ frequency $ L.map fst dataSet
       -- Names of the database without repetitions.
       matrix = wordMatrix dataSet keywordsList names
       -- Matrix of frecuency of the keywords for each name.
       (eigenvalues, eigenvectors) = eigenDecomposition matrix
       -- Eigen decomposition of the prior matrix.
       numComponents = numPrincipalComponents eigenvalues threshold
       -- Number of principal components needed to explain a given variance.
       pcaMatrix = principalComponentAnalysis eigenvectors matrix numComponents
       -- Result of performing Principal Component Analysis on the matrix.
       nameIndex = fromMaybe (error $ "Error: The item " ++ name ++ " was not found in the dataset.") (elemIndex name names)
       -- Index of the given name (the one we are fetching recommendation) on the list names.
       squaredDistances = LA.toList $ squaredEuclideanDistancesByRow pcaMatrix nameIndex
       -- A list of the distances from each name to the given name.
       closestIndices = orderedIndexes squaredDistances
       -- A list of the indexes of the list of names ordered by distance to the given name.
   in L.map (\index -> genericIndex names index) 
            (take n (L.filter (/= nameIndex) closestIndices))
       -- The list of the names which the indexes refered, in other words:
       -- The names of the database ordered by proximity with the given name.
