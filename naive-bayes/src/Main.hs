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
type ChickenStats = (Int, Int, BL.ByteString)
type SynthStats = (BL.ByteString, Double, Double, Int)

synthStats :: BL.ByteString -> Records SynthStats
synthStats = decode NoHeader

chickenStats :: BL.ByteString -> Records ChickenStats
chickenStats = decode NoHeader

fourth :: (a,b,c,d) -> d
fourth (_,_,_,d) = d

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

main :: IO ()
main = do
    csvData <- BL.readFile "batting.csv"
    let sums = F.foldr mapper M.empty (baseballStats csvData)
    let total = foldr totaler 0 sums
    let bayesTestList = sumMapToStats total sums
    let cls = bayes bayesTestList [150] -- Arbitrary values not corresponding to any
                                            -- real player

    -- Testing with synthetic data works I guess
    synthData <- BL.readFile "synth.te.csv"
    let synthsums = F.foldr synth M.empty (synthStats synthData)
    let synthtot = foldr totaler 0 synthsums
    let synthTest = sumMapToStats synthtot synthsums
    let normedChick = fmap (normFold [0.25,0.25]) synthTest
    let feed = bayes synthTest [(-0.45),0.825]
    putStrLn $ "synth total" ++ (show synthtot)
    putStrLn $ "synthen stuff " ++ (show synthTest)
    
    -- These are the intermediate lists/calculations. Useful for testing.
    --let list = M.toList sums
    --let cleanedList = cleanList list
    --let statList = map (sumsToStats (fromIntegral total)) cleanedList
    --let normFolded = fmap (normFold [1900,50]) statList

    -- Intermediate print statements.
    --putStrLn $ "Pre-cleaning \n" ++ (show list)
    --putStrLn $ "Post-cleaning \n" ++ (show cleanList)
    --putStrLn $ "After statification \n" ++ (show statList)
    --putStrLn $ "After norming (this would be done by bayes \n" ++ (show normFolded)
    
    putStrLn $ "class was " ++ (show feed)
    putStrLn $ "team was " ++ (show cls)

    -- Almost by necessity you need a function to unpack the data to feed it into your
    -- functions. I felt that tuple unpacking was simpler than learning cassava further.
    -- It's also convenient to just define these functions in a where statement I guess.
    where mapper (_,_,team,atBats) = applyDatum team (fromIntegral atBats)
          -- When I use years I end up getting NaN's going from sums to stats.
          mapper2 (_,year,team,atBats) = applyData team [year,atBats] -- atBats and Years
          chickmap (_,wgt,feed) = applyDatum feed (fromIntegral wgt)
          synth (_,x,y,c) = applyData c [x,y]
          totaler (val,_) = (+) val -- Totals

---------------------
-- FORMATTING DATA --
---------------------

-- Function that takes the map with key as the class and value as the summed attribute data
-- and translates that into 
sumMapToStats total = (map (sumsToStats (total)) . cleanList . M.toList)

-- Special function for "cleaning" the tuples produced in this
cleanTuple :: (a,(b,c)) -> (a,b,c)
cleanTuple (a,(b,c)) = (a,b,c)

cleanList :: (Functor f) => f (a,(b,c)) -> f (a,b,c)
cleanList = fmap cleanTuple

-- varFromMeanPair take Integrals by necessity because the input from the baseball
-- data was Int. This is bad, but I'm not sure how to generalize it. My Type fu is 
-- too weak.
sumsToStats :: (Floating a) => a -> (b, a, [(a,a)]) -> (b, a, [(a,a)])
sumsToStats tot (cls, num, prb) = (cls, num/tot, (map (varMeanFromPair num) prb))

-- takes a parameter and the value at a certain key, applies a function to those
-- and then sets that as the value at that key.
-- Incredibly surprised this wasn't a predefined function. All proper map functions
-- required that the supplied function take the same input as the maps values.
applyToKey :: (Ord k) => (a -> Maybe b -> b) -> k -> a -> M.Map k b -> M.Map k b
applyToKey func key val map = M.insert key (func val (M.lookup key map)) map

-- Takes a value and a pair of (sum,sigma(x^2)) and adds another value to it
sumAndSqSum :: Num a => a -> (a,a) -> (a,a)
sumAndSqSum num (oldSum, oldSqSum) = (oldSum + num, oldSqSum + num^2)

-- Special case for if you only have a single attribute to apply
summer :: (Num a) => a -> Maybe (a,[(a,a)]) -> (a,[(a,a)])
summer val Nothing = (1,[(val,val^2)])
summer val (Just (num,list)) = (num+1,fmap (sumAndSqSum val) list)

-- Sums a list of elements and counts the number of elements
-- This would be used if you had more than one attributes to sum up
listSummer :: (Num a) => [a] -> Maybe (a,[(a,a)]) -> (a,[(a,a)])
listSummer vals Nothing = (1,initList)
    where initList = fmap (\x -> (x,x^2)) vals -- In case of an empty key
listSummer vals (Just (num,list)) = (num+1,zipWith sumAndSqSum vals list)


-- These just help me understand what's happening. In a more general case you
-- can just curry applyToKey with whatever function you want
-- Type inference is incredible
type SummedData k a = M.Map k (a,[(a,a)])

applyDatum ::
    (Ord k, Num a) => k -> a -> SummedData k a -> SummedData k a
applyDatum = applyToKey summer

applyData ::
    (Ord k, Num a) => k -> [a] -> SummedData k a -> SummedData k a
applyData = applyToKey listSummer

----------------------------
-- NAIVE BAYES CLASSIFIER --
----------------------------

type Statistics = (Double, Double) -- (mean, var)
type Classifier a = (a, Double, [Statistics])
-- Wow! Type inference is amazing. I assumed that because I added a type parameter it
-- would need to be specified everywhere but apparently it _just works_
-- I should still maybe consider making this a data type

-- Helper for selector
maxPairr :: (Ord a) => (b, a) -> (b, a) -> (b, a)
maxPairr (x, a) (y, b)
    | a > b     = (x, a)
    | a < b     = (y, b)
    | otherwise = (x, a)

-- Selects the classifier with the greatest pd from a list of them
selector :: (Foldable f) => f (s, Double) -> s
selector pairs = fst (foldl1 maxPairr pairs)

-- Finds the normal probability density function.
pdfNorm :: (Floating a) => a -> (a,a) -> a
pdfNorm val (mean, var) = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))

normFold :: [Double] -> Classifier a -> (a, Double)
normFold atts (cls, prob, attstats) = (cls, normed)
    where normed = foldl (*) prob (zipWith pdfNorm atts attstats)

-- Given a list of classifiers and datum, will attempt to classify the datum.
bayes :: (Functor f, Foldable f) => f (Classifier a) -> [Double] -> a
bayes classes atts = selector (fmap (normFold atts) classes)

-- This is a really handsome function :)
mean :: (Floating a, Functor f, Foldable f) => f a -> a
mean xs = (sum xs) / (fromIntegral (length xs))

-----------------
-- PROBABILITY --
-----------------

meanFromSum elements sum = sum / elements
varFromSums elements sum sumSquared = (sumSquared - (sum^2)/elements) / (elements - 1)

varMeanFromSums :: (Floating a) => a -> a -> a -> (a,a)
varMeanFromSums n sum sumSq = 
    (meanFromSum n (sum),varFromSums n (sum) (sumSq))

varMeanFromPair :: (Floating a) => a -> (a,a) -> (a,a)
varMeanFromPair n (sum,sumSq) = varMeanFromSums n sum sumSq
