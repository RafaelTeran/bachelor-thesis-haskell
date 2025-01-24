-- | Module: Classifier
-- | Description: Implementation of a Naive Bayes cassifier.
-- | Functions:
-- | - classifierClasses: Explicit version of the classifier.
-- | - classifier: Implicit version of the classifier.

module Classifier where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord

-- Frequency function: Calculates the frequency of elements in a list.
frequency :: (Eq k, Hashable k, Integral v) => [k] -> HashMap k v
frequency [] = HM.empty
frequency (x:xs) = HM.insertWith (+) x 1 (frequency xs)

-- Frequency of each class in the dataset.
classFrequency :: [(String, String)] -> HashMap String Double
classFrequency dataSet =
  HM.map fromIntegral (frequency (L.map fst dataSet))

-- Probability of a given class based on its frequency.
classProbability :: String -> Double -> HashMap String Double -> Double
classProbability className totalEntries frequencies =
  lookupDefault 0 className (HM.map (/ totalEntries) frequencies)

-- Frequency of words for a specific class.
wordFrequencyByClass :: [(String, String)] -> String -> HashMap String Integer
wordFrequencyByClass dataSet className =
   frequency (concatMap words (L.map snd
              (L.filter (\entry -> className == fst entry) dataSet)))

-- Smoothed probability of a word given a class.
smoothedWordProbability :: String -> String
                        -> [(String, String)] -> Double -> Double
smoothedWordProbability word className dataSet totalWords =
   let wordFreqInClass = wordFrequencyByClass dataSet className
       totalWordsInClass = fromInteger $ sum $ HM.elems wordFreqInClass
       wordCountInClass = fromInteger $
                          lookupDefault 0 word wordFreqInClass
   in (wordCountInClass + 1) / (totalWordsInClass + totalWords)

-- Likelihood of a class given a text.
likelihoodClassGivenText :: String -> String -> Double ->
        HashMap String Double -> [(String, String)] -> Double -> Double
likelihoodClassGivenText className text totalWords frequencies dataSet totalEntries =
    let wordProbs = L.map (\word -> smoothedWordProbability word className 
                                          dataSet totalWords) (words text)
        classProb = classProbability className totalEntries frequencies
    in classProb * product wordProbs

-- Naive Bayes classifier: Explicit version.
-- Required: -- text: text to classify.
             -- dataSet: list of texts and their class.
             -- frequencies: frequency of each class in the dataSet. (to not be calculated for each class).
             -- classes: list of the names of the classes.
             -- totalWords: number of words of the dataSet without repetitions.
             -- totalEntries: numer of elements in the dataSet.
classifierClasses :: String -> [(String, String)] -> HashMap String Double 
          -> [String] -> Double -> Double -> [(String, Double)]
classifierClasses text dataSet frequencies classes totalWords totalEntries =
  L.map (\className -> (className, likelihoodClassGivenText className text totalWords
                                           frequencies dataSet totalEntries)) classes

-- Naive Bayes classifier: Implicit version.
-- Uses the explicit calculating frequencies, classes, totalWords and totalEntries from dataSet
-- Required: -- text: text to classify.
             -- dataSet: list of texts and their class.
classifier :: String -> [(String, String)] -> String
classifier text dataSet =
     let frequencies = classFrequency dataSet
         classes = HM.keys $ (frequency . L.map fst) dataSet
         totalWords = fromInteger $ sum $ HM.elems
                      ((frequency . concatMap words) (L.map snd dataSet))
         totalEntries = sum $ HM.elems frequencies
         probabilities = classifierClasses text dataSet frequencies 
                                          classes totalWords totalEntries        
     in fst (L.maximumBy (comparing snd) probabilities)
