-- Polynomial multiplication
-- | polynomial representation in list of tuples: (coefficient, exponent)
import Data.List
import Data.Ord
-- testdata
poly1=[(6,1),(2,2),(3,3),(1,4)]
poly2=[(1,2),(2,4)]
expected=[(6,3),(2,4),(15,5),(5,6),(6,7),(2,8)]
-- | sumfst sums fst tuple elements of a given list of tuples
sumfst :: [(Integer, Integer)] -> Integer
sumfst lst = sum $ map (\x -> fst x) lst
-- | multPoly takes two polynoms and returns the product.
-- multiply each coefficient in poly1 with each coefficient in poly2 and add exponents
multPoly :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multPoly lst1 lst2 = filter (\h -> fst h /= 0) $ nubBy (\f g -> f == g) $ concat (map (\d -> map (\e -> (sumfst d, snd e)) d) $ groupBy (\b c -> (snd b) == (snd c)) $ sortBy (comparing snd) $ concat $ foldr (\a -> ((foldr (\x -> (((fst x)*(fst a),(snd x)+(snd a)) :)) [] lst1) : )) [] lst2)