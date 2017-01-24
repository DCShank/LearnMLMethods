module Main where

import Data.List (sort)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map as M
-- from cassava
import Data.Csv

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

main :: IO ()
main = do
    csvData <- BL.readFile "batting.csv"
    let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
    -- let summed = fmap (V.foldr summer 0) v
    let summed = v >>= \x -> Right (V.foldr summer 0 x) -- Testing bind
    let teams = fmap (sort . V.foldr teamGet []) v
    putStrLn $ "Total atBats was: " ++ (show summed)
    putStrLn $ "Teams were: " ++ (show teams)
    where summer (name, year, team, atBats) n = n + atBats
          teamGet (name, year, team, atVats) xs = uniqueAdd team xs

-- Maybe use sets instead of this, or maybe there's some 
-- other predefined function that achieves the same thing.
-- Could be refactored to make a vector instead?
uniqueAdd :: (Eq a, Ord a) => a -> [a] -> [a]
uniqueAdd item aList
    | item `elem` aList = aList
    | otherwise         = item : aList

-- A little search into the docs indicates that this is actually done by a function
-- called genericLength. Strange.
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

pdfNorm :: (Floating a) => a -> (a,a) -> a
pdfNorm val (mean, var) = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))

-- Type names so I understand what I'm doing
-- Or maybe it's so I dont understand
-- Does abstraction promote understanding or reduce it?
--
-- This should all have been done using Data! Whoops!
-- Would have been very convenient
type Statistics     = (Float, Float) -- (Mean, Var)
type AttributeData  = V.Vector Float
type ClassifierData = (String, Float, V.Vector AttributeData)
type Classifier     = (String, Float, V.Vector Statistics)

classifierStats :: ClassifierData -> Classifier
classifierStats (cls, prob, atts) = (cls, prob, statCalc)
    where statCalc= fmap (meanVar) atts

maxPairr :: (Ord a) => (b, a) -> (b, a) -> (b, a)
maxPairr (x, a) (y, b)
    | a > b     = (x, a)
    | a < b     = (y, b)
    | otherwise = (x, a)

-- Selects the classifier with the greatest pdfthing from a list of them
selector :: (Foldable f, Functor f) => f (String, Float) -> String
selector pairs = fst (foldl1 maxPairr pairs)

--normFold :: (Functor f) => Classifier -> f Float -> (String, Float)
--normFold (cls, prob, attstats) atts = (cls, normed)
--    where normed = foldl (*) (zipWith pdfNorm atts attstats) prob

normFold :: V.Vector Float -> Classifier -> (String, Float)
normFold atts (cls, prob, attstats) = (cls, normed)
    where normed = foldl (*) prob (V.zipWith pdfNorm atts attstats)

bayes :: (Functor f, Foldable f) => f Classifier -> V.Vector Float -> String
bayes classes atts = selector (fmap (normFold atts) classes)
