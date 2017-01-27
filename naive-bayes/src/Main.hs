module Main where

import Data.List (sort)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
-- from cassava
import Data.Csv.Streaming

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

fourth :: (a,b,c,d) -> d
fourth (_,_,_,d) = d

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

main :: IO ()
main = do
    csvData <- BL.readFile "batting.csv"
    let summed = F.foldr summer 0 (baseballStats csvData)
    let sums = F.foldr mapper M.empty (baseballStats csvData)
    let total = foldr totaler 0 sums
    let list = M.toList sums
    --putStrLn $ "preCleaning " ++ (show list)
    let cleanList = fmap cleanTuple list
    --putStrLn $ "stuffs " ++ (show cleanList)
    let statList = map (sumsToStats (fromIntegral total)) cleanList
    --putStrLn $ "these numbers " ++ (show statList)
    let cls1 = bayes statList [1000000]
    let cls2 = bayes statList [1]
    let cls3 = bayes statList [200]
    --let normFolded = fmap (normFold [50]) statList
    --putStrLn $ "after norming " ++ (show normFolded)
    
    --putStrLn $ "Total atBats was: " ++ (show summed)
    putStrLn $ "team was" ++ (show cls1) ++ (show cls2) ++ (show cls3)
    where summer = (+) . fourth
          mapper (_,_,team,atBats) = applyDatum team atBats
          totaler (players,_) = (+) players

---------------------
-- FORMATTING DATA --
---------------------

-- Special function for "cleaning" the tuples produced in this
cleanTuple :: (Integral b, Num d) => (a,(b,c)) -> (a,d,c)
cleanTuple (a,(b,c)) = (a,fromIntegral b,c)

cleanList :: (Integral b, Num d) => [(a,(b,c))] -> [(a,d,c)]
cleanList = map cleanTuple

sumsToStats :: (Floating a, Integral b) => a -> (c, a, [(b,b)]) -> (c, a, [(a,a)])
sumsToStats tot (cls, num, prb) = (cls, num/tot, (map (varMeanFromPair num) prb))

applyToKey :: (Ord k) => (a -> Maybe b -> b) -> k -> a -> M.Map k b -> M.Map k b
applyToKey func key val map = M.insert key (func val (M.lookup key map)) map

sumAndSqSum :: Num a => a -> (a,a) -> (a,a)
sumAndSqSum num (oldSum, oldSqSum) = (oldSum + num, oldSqSum + num^2)

summer :: (Num a) => a -> Maybe (a,[(a,a)]) -> (a,[(a,a)])
summer val Nothing = (1,[(val,val^2)])
summer val (Just (num,list)) = (num+1,fmap (sumAndSqSum val) list)

applyDatum ::
    (Ord k, Num a) => k -> a -> M.Map k (a,[(a,a)]) -> M.Map k (a,[(a,a)])
applyDatum = applyToKey summer

----------------------------
-- NAIVE BAYES CLASSIFIER --
----------------------------

type Statistics = (Float, Float) -- (mean, var)
type Classifier a = (a, Float, [Statistics])
-- Wow! Type inference is amazing. I assumed that because I added a type parameter it
-- would need to be specified everywhere but apparently it _just works_
-- I should still maybe consider making this a data type

-- Helper for selector
maxPairr :: (Ord a) => (b, a) -> (b, a) -> (b, a)
maxPairr (x, a) (y, b)
    | a > b     = (x, a)
    | a < b     = (y, b)
    | otherwise = (x, a)

-- Selects the classifier with the greatest pdfthing from a list of them
selector :: (Foldable f) => f (s, Float) -> s
selector pairs = fst (foldl1 maxPairr pairs)

pdfNorm :: (Floating a) => a -> (a,a) -> a
pdfNorm val (mean, var) = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))

normFold :: [Float] -> Classifier a -> (a, Float)
normFold atts (cls, prob, attstats) = (cls, normed)
    where normed = foldl (*) prob (zipWith pdfNorm atts attstats)

-- Given a list of classifiers and datum, will attempt to classify the datum.
bayes :: (Functor f, Foldable f) => f (Classifier a) -> [Float] -> a
bayes classes atts = selector (fmap (normFold atts) classes)


mean :: (Floating a, Functor f, Foldable f) => f a -> a
mean xs = (sum xs) / (fromIntegral (length xs))

-----------------
-- PROBABILITY --
-----------------

meanFromSum elements sum = sum / elements
varFromSums elements sum sumSquared = (sumSquared - (sum^2)/elements) / (elements - 1)

varMeanFromSums :: (Floating b, Integral a) => b -> a -> a -> (b,b)
varMeanFromSums n sum sumSq = 
    (meanFromSum n (fromIntegral sum),varFromSums n (fromIntegral sum) (fromIntegral sumSq))

varMeanFromPair :: (Floating b, Integral a) => b -> (a,a) -> (b,b)
varMeanFromPair n (sum,sumSq) = varMeanFromSums n sum sumSq
