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
    let sums = F.foldr mapper M.empty (baseballStats csvData) ::
                            M.Map BL.ByteString (Int, [(Integer,Integer)])
    let total = foldr totaler 0 sums
    let list = M.toList sums
    --putStrLn $ "preCleaning " ++ (show list)
    let cleanList = fmap cleanTuple list
    --putStrLn $ "stuffs " ++ (show cleanList)
    let statList = map (statifyCorrect (fromIntegral total)) cleanList
    --putStrLn $ "these numbers " ++ (show statList)
    let cls = bayes statList [1000000]
    --let normFolded = fmap (normFold [50]) statList
    --putStrLn $ "after norming " ++ (show normFolded)
    
    --putStrLn $ "Total atBats was: " ++ (show summed)
    putStrLn $ "team was" ++ (show cls)
    where summer = (+) . fourth
          mapper (_,_,team,atBats) = applyDatum team atBats
          totaler (players,_) = (+) players

meanFromSum elements sum = sum / elements


varFromSums elements sum sumSquared = (sumSquared - (sum^2)/elements) / (elements - 1)

varMeanFromSums :: (Floating b, Integral a) => b -> a -> a -> (b,b)
varMeanFromSums n sum sumSq = 
    (meanFromSum n (fromIntegral sum),varFromSums n (fromIntegral sum) (fromIntegral sumSq))

varMeanFromPair :: (Floating b, Integral a) => b -> (a,a) -> (b,b)
varMeanFromPair n (sum,sumSq) = varMeanFromSums n sum sumSq

relativeProb :: (Floating a) => a -> a -> a
relativeProb elements allElements = elements / allElements

pdfNorm :: (Floating a) => a -> (a,a) -> a
pdfNorm val (mean, var) = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))

-- Special function for cleaning the tuples produced in this
cleanTuple :: (Integral b, Num d) => (a,(b,c)) -> (a,d,c)
cleanTuple (a,(b,c)) = (a,fromIntegral b,c)

cleanList :: (Integral b, Num d) => [(a,(b,c))] -> [(a,d,c)]
cleanList = map cleanTuple

type BS = BL.ByteString
statifyCorrect :: (Floating a, Integral b) => a -> (BS, a, [(b,b)]) -> (BS, a, [(a,a)])
statifyCorrect tot (cls, num, prb) = (cls, num/tot, (map (varMeanFromPair num) prb))

-- Frustrated by the tools for maps. This seems like a pretty simple function to
-- want but it was completely absent. Maybe it has something to do with purity?
-- like, this is pretty much just mutating the data inside the map.
-- Maybe that means I should be doing this some other way?
applyToKey :: (Ord k) => (a -> Maybe b -> b) -> k -> a -> M.Map k b -> M.Map k b
applyToKey func key val map = M.insert key (func val (M.lookup key map)) map

-- There must to be a better way to of be doing this
statter :: (Integral a, Integral b) => a -> Maybe (a,[(b,b)]) -> (a,[(b,b)])
statter val Nothing = (1,[(vali,vali^2)])
    where vali = fromIntegral val
statter val (Just (num,[(sum,sumsq)])) = (num+1,[(sum+vali,sumsq+vali^2)])
    where vali = fromIntegral val
fI = fromIntegral
-- Jesus christ this situation is gross.
-- I'm doing it in this assbackwards way because I have this idea that
-- maybe it should be generalizable, so the functions need to give a list of
-- sufficient statistics for each attribute and also the total number of elements.
--
-- For this case we only have one interesting attribute, but if we were to do
-- more this should be workable using this method.

applyDatum ::
    (Ord k, Integral a, Integral b) => k -> a -> M.Map k (a,[(b,b)]) -> M.Map k (a,[(b,b)])
applyDatum = applyToKey statter

----------------------------
-- NAIVE BAYES CLASSIFIER --
----------------------------

type Statistics = (Float, Float) -- (mean, var)
type Classifier a = (a, Float, [Statistics])

-- Helper for selector
maxPairr :: (Ord a) => (b, a) -> (b, a) -> (b, a)
maxPairr (x, a) (y, b)
    | a > b     = (x, a)
    | a < b     = (y, b)
    | otherwise = (x, a)

-- Selects the classifier with the greatest pdfthing from a list of them
selector :: (Foldable f) => f (s, Float) -> s
selector pairs = fst (foldl1 maxPairr pairs)

normFold :: [Float] -> Classifier a -> (a, Float)
normFold atts (cls, prob, attstats) = (cls, normed)
    where normed = foldl (*) prob (zipWith pdfNorm atts attstats)


-- Given a list of classifiers and datum, will attempt to classify the datum.
bayes :: (Functor f, Foldable f) => f (Classifier a) -> [Float] -> a
bayes classes atts = selector (fmap (normFold atts) classes)



mean :: (Floating a, Functor f, Foldable f) => f a -> a
mean xs = (sum xs) / (fromIntegral (length xs))
