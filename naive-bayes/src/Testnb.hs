import qualified Data.Vector as V
import qualified Data.List as L
type Classifier = [V.Vector Integer] -> Integer
trainNB :: [(V.Vector Integer, Integer)] -> Classifier
trainNB = undefined

syntheticData :: [(V.Vector Integer, Integer)]
syntheticData = [(V.fromList [0, 1, 2], 0),
                 (V.fromList [1, 0, 1], 1),
                 (V.fromList [1, 0, 0], 0)]

outcomeCounts :: [(V.Vector Integer, Integer)] -> [(Integer, Integer)]
outcomeCounts rows = [(value, countIn value labels) | value <- outcomes]
                    where outcomes = L.nub labels
                          labels = [snd row | row <- rows]
                          countIn somevalue alist = fromIntegral . length $ filter (==somevalue) alist
