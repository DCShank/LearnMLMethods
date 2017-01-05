module Main where

import Data.List (sort)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- from cassava
import Data.Csv

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

main :: IO ()
main = do
    csvData <- BL.readFile "batting.csv"
    let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
    let summed = fmap (V.foldr summer 0) v
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

pdfNorm :: (Floating a) => (a,a) -> a -> a
pdfNorm (mean, var) val = firstFac * expFac
    where firstFac = 1 / (sqrt ( 2 * pi * var))
          expFac   = exp ((-(val-mean) ** 2) / (2*var))
