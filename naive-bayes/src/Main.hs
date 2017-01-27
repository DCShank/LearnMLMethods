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
    --let summed = F.foldr summer 0 (baseballStats csvData)
    let sums = F.foldr mapper M.empty (baseballStats csvData) ::
                            M.Map BL.ByteString (Int, [(Integer,Integer)])
    let total = foldr totaler 0 sums
    let list = M.toList sums
    let cleanList = fmap cleanTuple list
    putStrLn $ "stuffs " ++ (show cleanList)
    let curriedStat = statify (fromIntegral total)
    let statList = map curriedStat cleanList
    let cls = bayes statList [1]
    let normFolded = fmap (normFold [50]) statList
    putStrLn $ "after norming " ++ (show normFolded)
    
    --putStrLn $ "Total atBats was: " ++ (show summed)
    putStrLn $ "team was" ++ (show cls)
    where summer = (+) . fourth
          mapper (_,_,team,atBats) = applyDatum team atBats
          totaler (players,_) = (+) players

meanFromSum elements sum = sum / elements
varFromSums elements sum sumSquared = (sumSquared - (sum^2)) / (elements - 1)
varMeanFromSums n sum sumSq = (meanFromSum n sum,varFromSums n sum sumSq)
varMeanFromPair :: (Floating b) => b -> (b,b) -> (b,b)
varMeanFromPair n (sum,sumSq) = varMeanFromSums n sum sumSq

relativeProb :: (Floating a) => a -> a -> a
relativeProb elements allElements = elements / allElements

pdfNorm :: (Floating a) => a -> (a,a) -> a
pdfNorm val (mean, var) = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))

cleanTuple :: (Integral b, Num d) => (a,(b,c)) -> (a,d,c)
cleanTuple (a,(b,c)) = (a,fromIntegral b,c)

statifyOne :: (Floating a, Integral b) => a -> (c,b,[(t,t)]) -> (c,a,[(t,t)])
statifyOne n (a,k,l) = (a,(fromIntegral k)/n,l)

statifyTwo :: (Floating a, Integral b) => a -> (c,a,[(b,b)]) -> (c,a,[(a,a)])
statifyTwo n (a,b,l) = 
    (a,b,(fmap (varMeanFromPair n) (map (\(x,y) -> (fromIntegral x, fromIntegral y)) l)))

statify n = (statifyTwo n) . (statifyOne n)

type Statistics = (Float, Float) -- (mean, var)
type Classifier = (BL.ByteString, Float, [Statistics])

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

-- Helper for selector
maxPairr :: (Ord a) => (b, a) -> (b, a) -> (b, a)
maxPairr (x, a) (y, b)
    | a > b     = (x, a)
    | a < b     = (y, b)
    | otherwise = (x, a)

-- Selects the classifier with the greatest pdfthing from a list of them
selector :: (Foldable f) => f (s, Float) -> s
selector pairs = fst (foldl1 maxPairr pairs)

--normFold :: (Functor f) => Classifier -> f Float -> (String, Float)
--normFold (cls, prob, attstats) atts = (cls, normed)
--    where normed = foldl (*) (zipWith pdfNorm atts attstats) prob

normFold :: [Float] -> Classifier -> (BL.ByteString, Float)
normFold atts (cls, prob, attstats) = (cls, normed)
    where normed = foldl (*) prob (zipWith pdfNorm atts attstats)

-- Given a list of classifiers and datum, will attempt to classify the datum.
bayes :: (Functor f, Foldable f) => f Classifier -> [Float] -> BL.ByteString
bayes classes atts = selector (fmap (normFold atts) classes)



-- Old stuff I no longer need for this project.

-- No longer needed!
uniqueAdd :: (Eq a, Ord a) => a -> [a] -> [a]
uniqueAdd item aList
    | item `elem` aList = aList
    | otherwise         = item : aList

-- A little search into the docs indicates that this is already done by a function
-- called genericLength.
nLength :: (Foldable t, Num b) => t a -> b
nLength = fromIntegral . length

-- It would probably be prudent to use a stats package for all the
-- stats stuff instead of these functions.
mean :: (Floating a, Functor f, Foldable f) => f a -> a
mean xs = (sum xs) / (nLength xs)

--variance :: (Floating a) => [a] -> a
--variance xs = sum (fmap varCalc xs) / (nLength xs - 1)
--    where varCalc x = (x - mean xs) ** 2

variance :: (Floating a, Functor f, Foldable f) => f a -> a
variance xs = ((expectedSquared) - (meanSquared)) / (nLength xs - 1)
    where expectedSquared = sum (fmap (\x -> x ** 2) xs)
          meanSquared = nLength xs * (mean xs) ** 2

meanVar :: (Floating a, Functor f, Foldable f) => f a -> (a,a)
meanVar xs = (mean xs, variance xs)

